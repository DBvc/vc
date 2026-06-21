open Cmdliner

type tool = Codex | Claude

type common_profile = {
  id : string;
  title : string;
  aliases : string list;
  env : (string * string) list;
  unset_env : string list;
}

type codex_profile = {
  common : common_profile;
  codex_profile : string;
}

type claude_profile = {
  common : common_profile;
  model : string;
}

type profile = Codex_profile of codex_profile | Claude_profile of claude_profile

type loaded_config = {
  path : string;
  exists : bool;
  profiles : profile list;
}

let ( let* ) value f = match value with Ok x -> f x | Error _ as e -> e

let with_context context = function
  | Ok value -> Ok value
  | Error message -> Error (context ^ ": " ^ message)

let getenv_opt name = try Some (Sys.getenv name) with Not_found -> None

let home_dir () = match getenv_opt "HOME" with Some home -> home | None -> "."

let config_path () =
  match getenv_opt "VC_AI_CONFIG" with
  | Some path -> path
  | None ->
      let config_home =
        match getenv_opt "XDG_CONFIG_HOME" with
        | Some path -> path
        | None -> Filename.concat (home_dir ()) ".config"
      in
      Filename.concat (Filename.concat config_home "vc") "ai_profiles.json"

let tool_to_string = function Codex -> "codex" | Claude -> "claude"
let tool_bin = tool_to_string
let tool_display_name = function Codex -> "Codex" | Claude -> "Claude"

let common = function Codex_profile p -> p.common | Claude_profile p -> p.common
let profile_tool = function Codex_profile _ -> Codex | Claude_profile _ -> Claude
let profile_id profile = (common profile).id
let profile_title profile = (common profile).title
let profile_aliases profile = (common profile).aliases
let profile_env profile = (common profile).env
let profile_unset_env profile = (common profile).unset_env

let starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix

let assoc_opt key fields = List.assoc_opt key fields

let allowed_field key allowed = List.exists (String.equal key) allowed

let ensure_allowed_fields context allowed fields =
  let unknown =
    fields |> List.map fst |> List.filter (fun key -> not (allowed_field key allowed))
  in
  match unknown with
  | [] -> Ok ()
  | keys ->
      Error
        (Printf.sprintf "%s: unknown field(s): %s" context (String.concat ", " keys))

let required_string key fields =
  match assoc_opt key fields with
  | Some (`String value) -> Ok value
  | Some _ -> Error ("field " ^ key ^ " must be a string")
  | None -> Error ("missing required field: " ^ key)

let optional_string key fields =
  match assoc_opt key fields with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error ("field " ^ key ^ " must be a string")

let optional_string_list key fields =
  match assoc_opt key fields with
  | None | Some `Null -> Ok []
  | Some (`List values) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String value :: rest -> loop (value :: acc) rest
        | _ -> Error ("field " ^ key ^ " must be a string list")
      in
      loop [] values
  | Some _ -> Error ("field " ^ key ^ " must be a string list")

let optional_env key fields =
  match assoc_opt key fields with
  | None | Some `Null -> Ok []
  | Some (`Assoc pairs) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, `String value) :: rest -> loop ((name, value) :: acc) rest
        | (name, _) :: _ -> Error ("env value for " ^ name ^ " must be a string")
      in
      loop [] pairs
  | Some _ -> Error ("field " ^ key ^ " must be an object")

let required_version fields =
  match assoc_opt "version" fields with
  | Some (`Int 1) -> Ok ()
  | Some (`Int value) -> Error (Printf.sprintf "unsupported config version: %d" value)
  | Some _ -> Error "field version must be an integer"
  | None -> Error "missing required field: version"

let tool_of_string = function
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | value -> Error ("unknown tool: " ^ value ^ ", expected codex or claude")

let ensure_tool_specific_fields context tool fields =
  match tool with
  | Codex ->
      if assoc_opt "model" fields <> None then
        Error (context ^ ": field model is not allowed for codex profiles; use codex_profile")
      else Ok ()
  | Claude ->
      if assoc_opt "codex_profile" fields <> None then
        Error (context ^ ": field codex_profile is not allowed for claude profiles; use model")
      else Ok ()

let parse_common fields =
  let* id = required_string "id" fields in
  let* title = optional_string "title" fields in
  let* aliases = optional_string_list "aliases" fields in
  let* env = optional_env "env" fields in
  let* unset_env = optional_string_list "unset_env" fields in
  Ok { id; title = (match title with Some value -> value | None -> id); aliases; env; unset_env }

let common_fields = [ "id"; "title"; "aliases"; "tool"; "env"; "unset_env" ]

let profile_of_yojson index json =
  match json with
  | `Assoc fields ->
      let context = Printf.sprintf "profiles[%d]" index in
      let* tool_value = required_string "tool" fields |> with_context context in
      let* tool = tool_of_string (String.lowercase_ascii tool_value) |> with_context context in
      let allowed =
        match tool with
        | Codex -> "codex_profile" :: common_fields
        | Claude -> "model" :: common_fields
      in
      let* () = ensure_tool_specific_fields context tool fields in
      let* () = ensure_allowed_fields context allowed fields in
      let* common = parse_common fields |> with_context context in
      (match tool with
      | Codex ->
          let* codex_profile = required_string "codex_profile" fields |> with_context context in
          Ok (Codex_profile { common; codex_profile })
      | Claude ->
          let* model = required_string "model" fields |> with_context context in
          Ok (Claude_profile { common; model }))
  | _ -> Error (Printf.sprintf "profiles[%d] must be an object" index)

let map_result_indexed f values =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest -> (
        match f index value with
        | Error e -> Error e
        | Ok x -> loop (index + 1) (x :: acc) rest)
  in
  loop 0 [] values

let ensure_unique_profile_ids profiles =
  let seen = Hashtbl.create 16 in
  let rec loop = function
    | [] -> Ok ()
    | profile :: rest ->
        let id = profile_id profile in
        if Hashtbl.mem seen id then Error ("duplicate profile id: " ^ id)
        else (
          Hashtbl.add seen id ();
          loop rest)
  in
  loop profiles

let parse_config path json =
  match json with
  | `Assoc fields ->
      let* () = ensure_allowed_fields path [ "version"; "profiles" ] fields in
      let* () = required_version fields in
      let* profiles_json =
        match assoc_opt "profiles" fields with
        | Some (`List values) -> Ok values
        | Some _ -> Error "field profiles must be a list"
        | None -> Error "missing required field: profiles"
      in
      let* profiles = map_result_indexed profile_of_yojson profiles_json in
      let* () = ensure_unique_profile_ids profiles in
      Ok profiles
  | _ -> Error (path ^ ": expected a JSON object")

let load_config () =
  let path = config_path () in
  if not (Sys.file_exists path) then Ok { path; exists = false; profiles = [] }
  else
    try
      let json = Yojson.Safe.from_file path in
      let* profiles = parse_config path json in
      Ok { path; exists = true; profiles }
    with exn -> Error (path ^ ": " ^ Printexc.to_string exn)

let profile_matches_tool tool_opt profile =
  match tool_opt with None -> true | Some tool -> profile_tool profile = tool

let profile_name_matches raw profile =
  profile_id profile = raw || List.exists (String.equal raw) (profile_aliases profile)

let resolve_profile profiles ?tool raw =
  let candidates = List.filter (profile_matches_tool tool) profiles in
  let matches = List.filter (profile_name_matches raw) candidates in
  match matches with
  | [ profile ] -> Ok profile
  | [] ->
      let names = candidates |> List.map profile_id |> String.concat ", " in
      if names = "" then Error ("unknown profile: " ^ raw ^ "\nno profiles configured")
      else Error ("unknown profile: " ^ raw ^ "\navailable: " ^ names)
  | profiles ->
      let names = profiles |> List.map profile_id |> String.concat ", " in
      Error ("ambiguous profile alias: " ^ raw ^ "\nmatched: " ^ names)

let is_env_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let env_refs s =
  let refs = ref [] in
  let len = String.length s in
  let add name = if name <> "" then refs := name :: !refs in
  let rec find_close j =
    if j >= len then None else if s.[j] = '}' then Some j else find_close (j + 1)
  in
  let rec loop i =
    if i >= len then ()
    else if s.[i] <> '$' then loop (i + 1)
    else if i + 1 < len && s.[i + 1] = '{' then
      match find_close (i + 2) with
      | None -> loop (i + 1)
      | Some j ->
          add (String.sub s (i + 2) (j - i - 2));
          loop (j + 1)
    else
      let j = ref (i + 1) in
      while !j < len && is_env_name_char s.[!j] do
        incr j
      done;
      if !j > i + 1 then (
        add (String.sub s (i + 1) (!j - i - 1));
        loop !j)
      else loop (i + 1)
  in
  loop 0;
  List.sort_uniq String.compare !refs

let profile_env_refs profile =
  profile_env profile
  |> List.concat_map (fun (_, value) -> env_refs value)
  |> List.sort_uniq String.compare

let expand_env s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec find_close j =
    if j >= len then None else if s.[j] = '}' then Some j else find_close (j + 1)
  in
  let add_env name =
    match getenv_opt name with Some value -> Buffer.add_string buf value | None -> ()
  in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] <> '$' then (
      Buffer.add_char buf s.[i];
      loop (i + 1))
    else if i + 1 < len && s.[i + 1] = '{' then
      match find_close (i + 2) with
      | None ->
          Buffer.add_char buf s.[i];
          loop (i + 1)
      | Some j ->
          add_env (String.sub s (i + 2) (j - i - 2));
          loop (j + 1)
    else
      let j = ref (i + 1) in
      while !j < len && is_env_name_char s.[!j] do
        incr j
      done;
      if !j > i + 1 then (
        add_env (String.sub s (i + 1) (!j - i - 1));
        loop !j)
      else (
        Buffer.add_char buf s.[i];
        loop (i + 1))
  in
  loop 0

let missing_env_vars profile =
  profile_env_refs profile |> List.filter (fun name -> getenv_opt name = None)

let contains_sub s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let rec loop i =
      i + len_sub <= len_s && (String.sub s i len_sub = sub || loop (i + 1))
    in
    loop 0

let is_secret_key key =
  let key = String.lowercase_ascii key in
  contains_sub key "token" || contains_sub key "key" || contains_sub key "secret"

let redact key value = if is_secret_key key then "<redacted>" else value

let is_safe_shell_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '/' | ':' | '=' | ',' ->
      true
  | _ -> false

let shell_quote value =
  if value = "" then "''"
  else if String.for_all is_safe_shell_char value then value
  else "'" ^ String.concat "'\\''" (String.split_on_char '\'' value) ^ "'"

let split_path path = path |> String.split_on_char ':' |> List.filter (fun item -> item <> "")

let is_executable path =
  try
    Unix.access path [ Unix.X_OK ];
    true
  with _ -> false

let which prog =
  if String.contains prog '/' then if is_executable prog then Some prog else None
  else
    let paths = match getenv_opt "PATH" with Some value -> split_path value | None -> [] in
    let rec loop = function
      | [] -> None
      | dir :: rest ->
          let path = Filename.concat dir prog in
          if is_executable path then Some path else loop rest
    in
    loop paths

let format_unix_error fn arg error =
  Printf.sprintf "%s %s: %s" fn arg (Unix.error_message error)

let env_array profile =
  let table = Hashtbl.create 128 in
  Array.iter
    (fun item ->
      match String.index_opt item '=' with
      | None -> ()
      | Some index ->
          let key = String.sub item 0 index in
          let value = String.sub item (index + 1) (String.length item - index - 1) in
          Hashtbl.replace table key value)
    (Unix.environment ());
  List.iter (fun key -> Hashtbl.remove table key) (profile_unset_env profile);
  List.iter
    (fun (key, value) -> Hashtbl.replace table key (expand_env value))
    (profile_env profile);
  Hashtbl.fold (fun key value acc -> (key ^ "=" ^ value) :: acc) table [] |> Array.of_list

let wait_status_to_exit = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED signal -> 128 + signal

let prompt_arg = function None | Some "" -> [] | Some prompt -> [ prompt ]

let command_for_profile profile prompt =
  match profile with
  | Codex_profile p -> [ "codex"; "--profile"; p.codex_profile ] @ prompt_arg prompt
  | Claude_profile p -> [ "claude"; "--model"; p.model ] @ prompt_arg prompt

let profile_display_label profile =
  Printf.sprintf "%s (%s)" (profile_title profile) (tool_display_name (profile_tool profile))

let print_launch profile args =
  Printf.eprintf "vc ai: %s\n" (profile_title profile);
  List.iter
    (fun (key, value) -> Printf.eprintf "env %s=%s\n" key (redact key (expand_env value)))
    (profile_env profile);
  List.iter (fun key -> Printf.eprintf "unset %s\n" key) (profile_unset_env profile);
  Printf.eprintf "$ %s\n%!" (String.concat " " (List.map shell_quote args))

let run_process ~dry_run profile args =
  if dry_run then (
    let missing = missing_env_vars profile in
    if missing <> [] then
      Printf.eprintf "vc ai: missing environment variable(s) for %s: %s\n" (profile_id profile)
        (String.concat ", " missing);
    print_launch profile args;
    0)
  else
    let missing = missing_env_vars profile in
    if missing <> [] then (
      Printf.eprintf "vc ai: missing environment variable(s) for %s: %s\n" (profile_id profile)
        (String.concat ", " missing);
      1)
    else
      let bin = tool_bin (profile_tool profile) in
      match which bin with
      | None ->
          Printf.eprintf "vc ai: executable not found in PATH: %s\n" bin;
          127
      | Some exe -> (
          print_launch profile args;
          try
            let pid =
              Unix.create_process_env exe (Array.of_list args) (env_array profile) Unix.stdin
                Unix.stdout Unix.stderr
            in
            let _, status = Unix.waitpid [] pid in
            wait_status_to_exit status
          with Unix.Unix_error (error, fn, arg) ->
            Printf.eprintf "vc ai: failed to run %s: %s\n" bin
              (format_unix_error fn arg error);
            1)

let prompt_from_args args =
  match args with [] -> None | values -> Some (String.concat " " values)

let split_profile_args args =
  match args with
  | [] -> Error "profile is required"
  | profile :: prompt_args -> Ok (profile, prompt_from_args prompt_args)

let launch ~dry_run ?tool args =
  match load_config () with
  | Error e ->
      Printf.eprintf "vc ai: %s\n" e;
      1
  | Ok loaded -> (
      match split_profile_args args with
      | Error e ->
          Printf.eprintf "vc ai: %s\n" e;
          1
      | Ok (raw_profile, prompt) -> (
          match resolve_profile loaded.profiles ?tool raw_profile with
          | Error e ->
              Printf.eprintf "vc ai: %s\n" e;
              1
          | Ok profile -> run_process ~dry_run profile (command_for_profile profile prompt)))

type picker_mode = Auto | Fzf | Builtin

let picker_mode_of_string = function
  | "auto" -> Ok Auto
  | "fzf" -> Ok Fzf
  | "builtin" -> Ok Builtin
  | value -> Error ("unsupported picker: " ^ value ^ ", expected auto, fzf, or builtin")

let tool_filter_of_string = function
  | "" -> Ok None
  | value ->
      let* tool = tool_of_string (String.lowercase_ascii value) in
      Ok (Some tool)

type picker_item = {
  index : int;
  profile : profile;
}

let picker_items ?tool profiles =
  profiles
  |> List.filter (profile_matches_tool tool)
  |> List.mapi (fun index profile -> { index = index + 1; profile })

let print_no_profiles loaded =
  Printf.eprintf "No AI profiles configured.\nConfig: %s\n" loaded.path;
  if not loaded.exists then Printf.eprintf "Run `vc ai sample-config` to see the schema.\n"

let print_picker_item item =
  let preview = command_for_profile item.profile None |> List.map shell_quote |> String.concat " " in
  Printf.eprintf "%d. %-28s %s\n" item.index (profile_display_label item.profile) preview

let sanitize_picker_field value =
  String.map (function '\t' | '\n' | '\r' -> ' ' | c -> c) value

let picker_item_preview item =
  command_for_profile item.profile None |> List.map shell_quote |> String.concat " "

let fzf_line item =
  Printf.sprintf "%d\t%s\t%s\n" item.index
    (sanitize_picker_field (profile_display_label item.profile))
    (sanitize_picker_field (picker_item_preview item))

let select_builtin items =
  match items with
  | [] -> Error (`Failure "no profiles available")
  | _ ->
      Printf.eprintf "Select AI profile:\n";
      List.iter print_picker_item items;
      Printf.eprintf "Choice [1-%d]: %!" (List.length items);
      let raw =
        try Some (read_line () |> String.trim)
        with End_of_file -> None
      in
      (match raw with
      | None | Some "" -> Error `Cancelled
      | Some value -> (
          match int_of_string_opt value with
          | None -> Error (`Failure ("invalid selection: " ^ value))
          | Some choice -> (
              match List.find_opt (fun item -> item.index = choice) items with
              | None -> Error (`Failure ("selection out of range: " ^ value))
              | Some item -> Ok item.profile)))

let close_fd_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_all_fd fd =
  let buffer = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_string buffer (Bytes.sub_string bytes 0 count);
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let write_all_fd fd value =
  let len = String.length value in
  let rec loop offset =
    if offset >= len then Ok ()
    else
      match Unix.write_substring fd value offset (len - offset) with
      | 0 -> Error "short write"
      | count -> loop (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
      | exception Unix.Unix_error (Unix.EPIPE, _, _) -> Error "broken pipe"
      | exception Unix.Unix_error (error, fn, arg) -> Error (format_unix_error fn arg error)
  in
  loop 0

let rec waitpid_nointr pid =
  try Unix.waitpid [] pid with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_nointr pid

let first_nonempty_line value =
  value |> String.split_on_char '\n' |> List.find_opt (fun line -> String.trim line <> "")

let parse_fzf_selection items output =
  match first_nonempty_line output with
  | None -> Error (`Failure "fzf returned no selection")
  | Some line ->
      let raw_index =
        match String.index_opt line '\t' with
        | None -> line
        | Some index -> String.sub line 0 index
      in
      (match int_of_string_opt (String.trim raw_index) with
      | None -> Error (`Failure ("fzf returned an invalid selection: " ^ line))
      | Some choice -> (
          match List.find_opt (fun item -> item.index = choice) items with
          | None -> Error (`Failure ("fzf selection out of range: " ^ raw_index))
          | Some item -> Ok item.profile))

let fzf_args = [| "fzf"; "--delimiter"; "\t"; "--with-nth"; "2,3"; "--prompt"; "vc ai> " |]

let run_fzf_process exe input =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  Unix.set_close_on_exec stdin_w;
  Unix.set_close_on_exec stdout_r;
  try
    let pid = Unix.create_process_env exe fzf_args (Unix.environment ()) stdin_r stdout_w Unix.stderr in
    close_fd_noerr stdin_r;
    close_fd_noerr stdout_w;
    let write_error =
      match write_all_fd stdin_w input with Ok () -> None | Error e -> Some e
    in
    close_fd_noerr stdin_w;
    let output =
      Fun.protect ~finally:(fun () -> close_fd_noerr stdout_r) (fun () -> read_all_fd stdout_r)
    in
    let _, status = waitpid_nointr pid in
    Ok (output, status, write_error)
  with Unix.Unix_error (error, fn, arg) ->
    close_fd_noerr stdin_r;
    close_fd_noerr stdin_w;
    close_fd_noerr stdout_r;
    close_fd_noerr stdout_w;
    Error (format_unix_error fn arg error)

let select_fzf_with_exe exe items =
  let input = items |> List.map fzf_line |> String.concat "" in
  match run_fzf_process exe input with
  | Error e -> Error (`Failure ("fzf failed: " ^ e))
  | Ok (output, status, write_error) -> (
      match status with
      | Unix.WEXITED 0 -> (
          match write_error with
          | Some e -> Error (`Failure ("fzf input failed: " ^ e))
          | None -> parse_fzf_selection items output)
      | Unix.WEXITED (1 | 130) -> Error `Cancelled
      | _ ->
          let detail =
            match write_error with None -> "" | Some e -> " (" ^ e ^ ")"
          in
          Error
            (`Failure
              (Printf.sprintf "fzf failed with exit code %d%s"
                 (wait_status_to_exit status) detail)))

let select_with_picker picker items =
  match picker with
  | Builtin -> select_builtin items
  | Fzf -> (
      match which "fzf" with
      | None -> Error (`Missing "fzf picker requested but fzf was not found in PATH")
      | Some exe -> select_fzf_with_exe exe items)
  | Auto -> (
      match which "fzf" with
      | None -> select_builtin items
      | Some exe -> select_fzf_with_exe exe items)

let has_interactive_tty () = Unix.isatty Unix.stdin && Unix.isatty Unix.stderr

let cmd_pick ~require_tty ~dry_run picker_raw tool_raw =
  match picker_mode_of_string picker_raw with
  | Error e ->
      Printf.eprintf "vc ai: %s\n" e;
      1
  | Ok picker ->
      if require_tty && not (has_interactive_tty ()) then (
        Printf.eprintf
          "vc ai: interactive picker requires a terminal; use `vc ai pick` or a subcommand like \
           `vc ai list`.\n";
        1)
      else (
        match tool_filter_of_string tool_raw with
        | Error e ->
            Printf.eprintf "vc ai: %s\n" e;
            1
        | Ok tool -> (
            match load_config () with
            | Error e ->
                Printf.eprintf "vc ai: %s\n" e;
                1
            | Ok loaded ->
                if loaded.profiles = [] then (
                  print_no_profiles loaded;
                  1)
                else
                  let items = picker_items ?tool loaded.profiles in
                  if items = [] then (
                    Printf.eprintf "vc ai: no profiles match selected tool\n";
                    1)
                  else
                    match select_with_picker picker items with
                    | Ok profile ->
                        run_process ~dry_run profile (command_for_profile profile None)
                    | Error (`Missing e) ->
                        Printf.eprintf "vc ai: %s\n" e;
                        127
                    | Error `Cancelled ->
                        Printf.eprintf "vc ai: selection cancelled\n";
                        130
                    | Error (`Failure e) ->
                        Printf.eprintf "vc ai: %s\n" e;
                        1))

type display_profile = {
  display_id : string;
  display_title : string;
  display_handle : string;
  display_target : string;
}

let matching_profile_count profiles name =
  List.fold_left
    (fun count profile -> if profile_name_matches name profile then count + 1 else count)
    0 profiles

let profile_handle profiles profile =
  let safe_alias alias = matching_profile_count profiles alias = 1 in
  match List.find_opt safe_alias (profile_aliases profile) with
  | Some alias -> alias
  | None -> profile_id profile

let profile_target = function
  | Codex_profile p -> p.codex_profile
  | Claude_profile p -> p.model

let display_profile profiles profile =
  {
    display_id = profile_id profile;
    display_title = profile_title profile;
    display_handle = profile_handle profiles profile;
    display_target = profile_target profile;
  }

let compare_display_profile left right =
  let compare_field get =
    String.compare (get left) (get right)
  in
  let title = compare_field (fun p -> p.display_title) in
  if title <> 0 then title
  else
    let handle = compare_field (fun p -> p.display_handle) in
    if handle <> 0 then handle else compare_field (fun p -> p.display_id)

let display_profiles_for_tool tool profiles =
  profiles
  |> List.filter (fun profile -> profile_tool profile = tool)
  |> List.map (display_profile profiles)
  |> List.sort compare_display_profile

let max_string_width minimum values =
  List.fold_left (fun width value -> max width (String.length value)) minimum values

let pad_right width value =
  let padding = width - String.length value in
  if padding <= 0 then value else value ^ String.make padding ' '

let target_header = function Codex -> "CODEX PROFILE" | Claude -> "MODEL ID"

let print_display_group tool profiles =
  let title_header = "TITLE" in
  let handle_header = "HANDLE" in
  let target_header = target_header tool in
  let title_width =
    profiles
    |> List.map (fun profile -> profile.display_title)
    |> max_string_width (String.length title_header)
  in
  let handle_width =
    profiles
    |> List.map (fun profile -> profile.display_handle)
    |> max_string_width (String.length handle_header)
  in
  let print_row title handle target =
    Printf.printf "  %s  %s  %s\n" (pad_right title_width title)
      (pad_right handle_width handle) target
  in
  Printf.printf "%s\n" (tool_display_name tool);
  print_row title_header handle_header target_header;
  List.iter
    (fun profile ->
      print_row profile.display_title profile.display_handle profile.display_target)
    profiles

let print_human_profile_list profiles =
  let groups =
    [ Codex; Claude ]
    |> List.map (fun tool -> (tool, display_profiles_for_tool tool profiles))
    |> List.filter (fun (_, profiles) -> profiles <> [])
  in
  let rec loop = function
    | [] -> ()
    | [ (tool, profiles) ] -> print_display_group tool profiles
    | (tool, profiles) :: rest ->
        print_display_group tool profiles;
        print_newline ();
        loop rest
  in
  loop groups

let profile_to_yojson profile =
  let common = common profile in
  let fields =
    [
      ("id", `String common.id);
      ("title", `String common.title);
      ("aliases", `List (List.map (fun value -> `String value) common.aliases));
      ("tool", `String (tool_to_string (profile_tool profile)));
      ("env_keys", `List (List.map (fun (key, _) -> `String key) common.env));
      ("unset_env", `List (List.map (fun value -> `String value) common.unset_env));
    ]
  in
  let tool_fields =
    match profile with
    | Codex_profile p -> [ ("codex_profile", `String p.codex_profile) ]
    | Claude_profile p -> [ ("model", `String p.model) ]
  in
  `Assoc (fields @ tool_fields)

let cmd_list json =
  match load_config () with
  | Error e ->
      Printf.eprintf "vc ai: %s\n" e;
      1
  | Ok loaded ->
      if json then (
        loaded.profiles |> List.map profile_to_yojson |> fun profiles ->
        `List profiles |> Yojson.Safe.pretty_to_string |> print_endline;
        0)
      else if loaded.profiles = [] then (
        Printf.printf "No AI profiles configured.\nConfig: %s\n" loaded.path;
        if not loaded.exists then Printf.printf "Run `vc ai sample-config` to see the schema.\n";
        0)
      else (
        print_human_profile_list loaded.profiles;
        0)

let config_profile_to_yojson profile =
  let common = common profile in
  let base =
    [
      ("id", `String common.id);
      ("title", `String common.title);
      ("aliases", `List (List.map (fun value -> `String value) common.aliases));
      ("tool", `String (tool_to_string (profile_tool profile)));
      ("env", `Assoc (List.map (fun (key, value) -> (key, `String value)) common.env));
      ("unset_env", `List (List.map (fun value -> `String value) common.unset_env));
    ]
  in
  let tool_fields =
    match profile with
    | Codex_profile p -> [ ("codex_profile", `String p.codex_profile) ]
    | Claude_profile p -> [ ("model", `String p.model) ]
  in
  `Assoc (base @ tool_fields)

let sample_profiles =
  [
    Codex_profile
      {
        common =
          {
            id = "codex-your-model";
            title = "Your Codex Model";
            aliases = [ "codex" ];
            env = [];
            unset_env = [];
          };
        codex_profile = "your-codex-profile";
      };
    Claude_profile
      {
        common =
          {
            id = "claude-your-model";
            title = "Your Claude Model";
            aliases = [ "claude" ];
            env =
              [
                ("ANTHROPIC_BASE_URL", "${YOUR_CLAUDE_BASE_URL}");
                ("ANTHROPIC_AUTH_TOKEN", "${YOUR_CLAUDE_API_TOKEN}");
              ];
            unset_env = [ "ANTHROPIC_API_KEY" ];
          };
        model = "your-model-name";
      };
  ]

let sample_config_json () =
  `Assoc
    [
      ("version", `Int 1);
      ("profiles", `List (List.map config_profile_to_yojson sample_profiles));
    ]
  |> Yojson.Safe.pretty_to_string

let cmd_sample_config () =
  sample_config_json () |> print_endline;
  0

let rec mkdir_p dir =
  if dir = "" || dir = "." then Ok ()
  else if Sys.file_exists dir then
    if Sys.is_directory dir then Ok () else Error (dir ^ " exists and is not a directory")
  else
    let parent = Filename.dirname dir in
    let* () = if parent = dir then Ok () else mkdir_p parent in
    try
      Unix.mkdir dir 0o700;
      Ok ()
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when Sys.file_exists dir && Sys.is_directory dir ->
        Ok ()
    | Unix.Unix_error (error, fn, arg) -> Error (format_unix_error fn arg error)
    | Sys_error message -> Error message

let write_file ~force path contents =
  try
    let flags =
      [ Open_wronly; Open_binary; Open_creat; (if force then Open_trunc else Open_excl) ]
    in
    let ch = open_out_gen flags 0o600 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr ch)
      (fun () -> output_string ch contents);
    Unix.chmod path 0o600;
    Ok ()
  with
  | Sys_error message -> Error message
  | Unix.Unix_error (error, fn, arg) -> Error (format_unix_error fn arg error)

let cmd_init_config force =
  let path = config_path () in
  if Sys.file_exists path && not force then (
    Printf.eprintf
      "vc ai: config already exists: %s\nUse `vc ai init-config --force` to overwrite.\n"
      path;
    1)
  else
    match mkdir_p (Filename.dirname path) with
    | Error message ->
        Printf.eprintf "vc ai: failed to create config directory for %s: %s\n" path message;
        1
    | Ok () -> (
        match write_file ~force path (sample_config_json () ^ "\n") with
        | Error message ->
            Printf.eprintf "vc ai: failed to write config %s: %s\n" path message;
            1
        | Ok () ->
            Printf.printf "%s AI profile config: %s\n" (if force then "Wrote" else "Created")
              path;
            0)

let print_binary_status tool =
  let bin = tool_bin tool in
  match which bin with
  | Some path -> Printf.printf "binary %s: found %s\n" bin path
  | None -> Printf.printf "binary %s: missing in PATH (warning)\n" bin

let read_file_opt path =
  try
    let ch = open_in_bin path in
    Some
      (Fun.protect
         ~finally:(fun () -> close_in_noerr ch)
         (fun () -> really_input_string ch (in_channel_length ch)))
  with _ -> None

let strip_surrounding_quotes value =
  let value = String.trim value in
  let len = String.length value in
  if
    len >= 2
    && ((value.[0] = '"' && value.[len - 1] = '"')
       || (value.[0] = '\'' && value.[len - 1] = '\''))
  then String.sub value 1 (len - 2)
  else value

let toml_assignment_value key line =
  let line = String.trim line in
  if line = "" || starts_with ~prefix:"#" line || starts_with ~prefix:"[" line then None
  else
    match String.index_opt line '=' with
    | None -> None
    | Some index ->
        let lhs = String.sub line 0 index |> String.trim in
        if lhs <> key then None
        else
          let rhs = String.sub line (index + 1) (String.length line - index - 1) in
          Some (strip_surrounding_quotes rhs)

let toml_value key text =
  text |> String.split_on_char '\n' |> List.find_map (toml_assignment_value key)

let toml_has_assignment key value text =
  match toml_value key text with Some found -> found = value | None -> false

let toml_has_legacy_profile_selector text =
  text |> String.split_on_char '\n'
  |> List.exists (fun line ->
         match toml_assignment_value "profile" line with Some _ -> true | None -> false)

let toml_has_legacy_profiles_table text =
  text |> String.split_on_char '\n'
  |> List.exists (fun line ->
         let line = String.trim line in
         starts_with ~prefix:"[profiles." line)

let toml_has_model_provider_section provider text =
  let section = "[model_providers." ^ provider ^ "]" in
  text |> String.split_on_char '\n'
  |> List.exists (fun line -> String.trim line = section)

let is_builtin_model_provider = function
  | "openai" | "ollama" | "lmstudio" | "amazon-bedrock" -> true
  | _ -> false

let effective_env_var profile key =
  match List.assoc_opt key (profile_env profile) with
  | Some value -> Some (expand_env value)
  | None ->
      if List.exists (String.equal key) (profile_unset_env profile) then None else getenv_opt key

let expand_home_path path =
  if path = "~" then home_dir ()
  else if starts_with ~prefix:"~/" path then
    Filename.concat (home_dir ()) (String.sub path 2 (String.length path - 2))
  else path

let effective_codex_home profile =
  match effective_env_var profile "CODEX_HOME" with
  | Some "" | None -> Filename.concat (home_dir ()) ".codex"
  | Some path -> expand_home_path path

let resolve_path_from_codex_home codex_home path =
  let path = expand_home_path path in
  if Filename.is_relative path then Filename.concat codex_home path else path

let print_file_status label path =
  Printf.printf "  %s: %s %s\n" label
    (if Sys.file_exists path then "found" else "missing")
    path

let print_codex_profile_doctor profile p =
  let codex_home = effective_codex_home profile in
  let main_config_path = Filename.concat codex_home "config.toml" in
  let profile_config_path = Filename.concat codex_home (p.codex_profile ^ ".config.toml") in
  let profile_sets_codex_home = List.assoc_opt "CODEX_HOME" (profile_env profile) <> None in
  let launch_sets_codex_home = effective_env_var profile "CODEX_HOME" <> None in
  Printf.printf "  codex home: %s\n" codex_home;
  if profile_sets_codex_home then
    Printf.printf
      "  codex warning: this vc profile sets CODEX_HOME; that changes Codex config/state roots\n"
  else if launch_sets_codex_home then
    Printf.printf
      "  codex warning: CODEX_HOME is set in the launch environment; bare Codex and vc ai may use \
       different config/state roots\n";
  print_file_status "codex main config" main_config_path;
  print_file_status "codex profile file" profile_config_path;
  let main_text = read_file_opt main_config_path in
  let profile_text = read_file_opt profile_config_path in
  Option.iter
    (fun text ->
      if toml_has_legacy_profile_selector text then
        Printf.printf
          "  codex warning: main config contains legacy profile = ... selector\n";
      if toml_has_legacy_profiles_table text then
        Printf.printf
          "  codex warning: main config contains legacy [profiles.*] tables\n")
    main_text;
  match profile_text with
  | None ->
      Printf.printf
        "  codex warning: codex --profile %s needs %s\n" p.codex_profile profile_config_path
  | Some text ->
      if toml_has_legacy_profiles_table text then
        Printf.printf
          "  codex warning: profile file contains legacy [profiles.*] tables\n";
      if toml_has_assignment "forced_login_method" "api" text then
        Printf.printf
          "  codex warning: forced_login_method=api can conflict with ChatGPT web login\n";
      let provider = toml_value "model_provider" text in
      let catalog = toml_value "model_catalog_json" text in
      Option.iter (fun value -> Printf.printf "  codex model_provider: %s\n" value) provider;
      (match catalog with
      | None -> ()
      | Some path ->
          let resolved = resolve_path_from_codex_home codex_home path in
          Printf.printf "  codex model_catalog_json: %s %s\n"
            (if Sys.file_exists resolved then "found" else "missing")
            resolved);
      (match provider with
      | Some provider when not (is_builtin_model_provider provider) ->
          let provider_defined =
            List.exists
              (fun text -> toml_has_model_provider_section provider text)
              (List.filter_map Fun.id [ main_text; profile_text ])
          in
          if not provider_defined then
            Printf.printf
              "  codex warning: model_provider=%s but [model_providers.%s] was not found\n"
              provider provider;
          if catalog = None then
            Printf.printf
              "  codex warning: custom model_provider has no model_catalog_json; Codex may use \
               fallback model metadata\n"
      | _ -> ())

let print_doctor_profile profile =
  Printf.printf "profile %s (%s): %s\n" (profile_id profile)
    (tool_to_string (profile_tool profile))
    (profile_title profile);
  (match profile_env profile with
  | [] -> Printf.printf "  env: none\n"
  | env ->
      List.iter
        (fun (key, value) ->
          Printf.printf "  env %s=%s\n" key (redact key (expand_env value)))
        env);
  (match profile_env_refs profile with
  | [] -> Printf.printf "  env refs: none\n"
  | refs ->
      List.iter
        (fun name ->
          match getenv_opt name with
          | Some _ -> Printf.printf "  env ref %s: set\n" name
          | None -> Printf.printf "  env ref %s: missing (warning)\n" name)
        refs);
  List.iter (fun key -> Printf.printf "  unset %s\n" key) (profile_unset_env profile);
  match profile with
  | Codex_profile p -> print_codex_profile_doctor profile p
  | Claude_profile _ -> ()

let cmd_doctor () =
  match load_config () with
  | Error e ->
      Printf.eprintf "vc ai: %s\n" e;
      1
  | Ok loaded ->
      Printf.printf "Config: %s\n" loaded.path;
      Printf.printf "Config file: %s\n" (if loaded.exists then "found" else "missing");
      Printf.printf "Profiles: %d\n" (List.length loaded.profiles);
      print_binary_status Codex;
      print_binary_status Claude;
      List.iter print_doctor_profile loaded.profiles;
      0

let args_pos doc = Arg.(value & pos_all string [] & info [] ~docv:"ARGS" ~doc)

let dry_run_flag =
  let doc = "Print the resolved command without launching it." in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let json_flag =
  let doc = "Print JSON output." in
  Arg.(value & flag & info [ "json" ] ~doc)

let force_flag =
  let doc = "Overwrite an existing ai_profiles.json." in
  Arg.(value & flag & info [ "force" ] ~doc)

let picker_arg =
  let doc = "Select a picker implementation: auto, fzf, or builtin." in
  Arg.(value & opt string "auto" & info [ "picker" ] ~docv:"PICKER" ~doc)

let tool_filter_arg =
  let doc = "Only show profiles for TOOL, either codex or claude." in
  Arg.(value & opt string "" & info [ "tool" ] ~docv:"TOOL" ~doc)

let list_cmd = Cmd.v (Cmd.info "list" ~doc:"List AI launcher profiles") Term.(const cmd_list $ json_flag)

let sample_config_cmd =
  Cmd.v (Cmd.info "sample-config" ~doc:"Print a sample ai_profiles.json")
    Term.(const cmd_sample_config $ const ())

let init_config_cmd =
  Cmd.v (Cmd.info "init-config" ~doc:"Create a sample ai_profiles.json")
    Term.(const cmd_init_config $ force_flag)

let doctor_cmd = Cmd.v (Cmd.info "doctor" ~doc:"Diagnose AI launcher configuration") Term.(const cmd_doctor $ const ())

let pick_cmd =
  Cmd.v (Cmd.info "pick" ~doc:"Pick and run an AI profile")
    Term.(const (fun dry_run picker tool -> cmd_pick ~require_tty:false ~dry_run picker tool) $ dry_run_flag
          $ picker_arg $ tool_filter_arg)

let default_term =
  Term.(
    const (fun dry_run picker tool -> cmd_pick ~require_tty:true ~dry_run picker tool)
    $ dry_run_flag $ picker_arg $ tool_filter_arg)

let run_cmd =
  Cmd.v (Cmd.info "run" ~doc:"Run a profile by id or alias")
    Term.(const (fun dry_run args -> launch ~dry_run args) $ dry_run_flag
          $ args_pos "PROFILE [PROMPT ...]")

let codex_cmd =
  Cmd.v (Cmd.info "codex" ~doc:"Run a Codex profile by id or alias")
    Term.(const (fun dry_run args -> launch ~dry_run ~tool:Codex args) $ dry_run_flag
          $ args_pos "PROFILE [PROMPT ...]")

let claude_cmd =
  Cmd.v (Cmd.info "claude" ~doc:"Run a Claude Code profile by id or alias")
    Term.(const (fun dry_run args -> launch ~dry_run ~tool:Claude args) $ dry_run_flag
          $ args_pos "PROFILE [PROMPT ...]")

let cmd =
  let doc = "Launch coding CLIs with explicit local profiles." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "ai is a lightweight launcher for Codex and Claude Code. It reads local profiles from \
         ai_profiles.json and does not include built-in model profiles.";
      `S Manpage.s_examples;
      `P "vc ai list";
      `P "vc ai sample-config";
      `P "vc ai init-config";
      `P "vc ai doctor";
      `P "vc ai";
      `P "vc ai pick --picker builtin";
      `P "vc ai codex --dry-run main";
      `P "vc ai claude --dry-run main";
    ]
  in
  Cmd.group ~default:default_term (Cmd.info "ai" ~version:"0.1.0" ~doc ~man)
    [
      list_cmd;
      sample_config_cmd;
      init_config_cmd;
      doctor_cmd;
      pick_cmd;
      run_cmd;
      codex_cmd;
      claude_cmd;
    ]
