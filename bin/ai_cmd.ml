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

let common = function Codex_profile p -> p.common | Claude_profile p -> p.common
let profile_tool = function Codex_profile _ -> Codex | Claude_profile _ -> Claude
let profile_id profile = (common profile).id
let profile_title profile = (common profile).title
let profile_aliases profile = (common profile).aliases
let profile_env profile = (common profile).env
let profile_unset_env profile = (common profile).unset_env

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
            Printf.eprintf "vc ai: failed to run %s: %s %s %s\n" bin fn arg
              (Unix.error_message error);
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

let target_label = function
  | Codex_profile p -> "codex_profile=" ^ p.codex_profile
  | Claude_profile p -> "model=" ^ p.model

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
        List.iter
          (fun p ->
            let aliases =
              match profile_aliases p with [] -> "" | xs -> " aliases=" ^ String.concat "," xs
            in
            Printf.printf "%-18s %-6s %-28s %s%s\n" (profile_id p)
              (tool_to_string (profile_tool p)) (target_label p) (profile_title p) aliases)
          loaded.profiles;
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
            id = "codex-main";
            title = "Codex / Main";
            aliases = [ "main" ];
            env = [];
            unset_env = [];
          };
        codex_profile = "main";
      };
    Claude_profile
      {
        common =
          {
            id = "claude-main";
            title = "Claude Code / Main";
            aliases = [ "main" ];
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

let format_unix_error fn arg error =
  Printf.sprintf "%s %s: %s" fn arg (Unix.error_message error)

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
  List.iter (fun key -> Printf.printf "  unset %s\n" key) (profile_unset_env profile)

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

let list_cmd = Cmd.v (Cmd.info "list" ~doc:"List AI launcher profiles") Term.(const cmd_list $ json_flag)

let sample_config_cmd =
  Cmd.v (Cmd.info "sample-config" ~doc:"Print a sample ai_profiles.json")
    Term.(const cmd_sample_config $ const ())

let init_config_cmd =
  Cmd.v (Cmd.info "init-config" ~doc:"Create a sample ai_profiles.json")
    Term.(const cmd_init_config $ force_flag)

let doctor_cmd = Cmd.v (Cmd.info "doctor" ~doc:"Diagnose AI launcher configuration") Term.(const cmd_doctor $ const ())

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
      `P "vc ai codex --dry-run main";
      `P "vc ai claude --dry-run main";
    ]
  in
  Cmd.group (Cmd.info "ai" ~version:"0.1.0" ~doc ~man)
    [ list_cmd; sample_config_cmd; init_config_cmd; doctor_cmd; run_cmd; codex_cmd; claude_cmd ]
