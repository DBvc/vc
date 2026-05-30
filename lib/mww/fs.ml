open Resultx

let path_exists = Sys.file_exists

let is_dir path =
  try (Unix.stat path).st_kind = Unix.S_DIR with Unix.Unix_error _ | Sys_error _ -> false

let is_file path =
  try (Unix.stat path).st_kind = Unix.S_REG with Unix.Unix_error _ | Sys_error _ -> false

let cwd () = Sys.getcwd ()
let absolutize path = if Filename.is_relative path then Filename.concat (cwd ()) path else path
let mkdir path = protect ~where:("mkdir " ^ path) (fun () -> Unix.mkdir path 0o755)

let rec mkdir_p path =
  let path = if path = "" then "." else path in
  if path_exists path then
    if is_dir path then Ok () else Error (path ^ " exists but is not a directory")
  else
    let parent = Filename.dirname path in
    let* () = if parent = path then Ok () else mkdir_p parent in
    mkdir path

let read_file path =
  protect ~where:("read " ^ path) (fun () ->
      let ch = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ch)
        (fun () -> really_input_string ch (in_channel_length ch)))

let write_file path contents =
  let* () = mkdir_p (Filename.dirname path) in
  protect ~where:("write " ^ path) (fun () ->
      let ch = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch contents))

let append_file path contents =
  let* () = mkdir_p (Filename.dirname path) in
  protect ~where:("append " ^ path) (fun () ->
      let ch = open_out_gen [ Open_creat; Open_text; Open_append ] 0o644 path in
      Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch contents))

let list_dirs path =
  if not (path_exists path) then Ok []
  else if not (is_dir path) then Error (path ^ " is not a directory")
  else
    protect ~where:("list " ^ path) (fun () ->
        Sys.readdir path |> Array.to_list
        |> List.filter (fun name -> is_dir (Filename.concat path name))
        |> List.sort String.compare)

let remove_file_if_exists path =
  if path_exists path then protect ~where:("remove " ^ path) (fun () -> Sys.remove path) else Ok ()

let rmdir_if_empty path =
  if path_exists path && is_dir path then
    protect ~where:("rmdir " ^ path) (fun () -> Unix.rmdir path)
  else Ok ()

let first_existing paths = List.find_opt path_exists paths

let current_user () =
  match Sys.getenv_opt "USER" with
  | Some user when user <> "" -> user
  | _ -> ( match Sys.getenv_opt "USERNAME" with Some user when user <> "" -> user | _ -> "user")

let rec find_up_from ~filename dir =
  let candidate = Filename.concat dir filename in
  if path_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up_from ~filename parent

let find_up ~filename = find_up_from ~filename (cwd ())
let dirname = Filename.dirname
let basename = Filename.basename
let concat = Filename.concat

let join parts =
  match parts with [] -> "." | first :: rest -> List.fold_left Filename.concat first rest
