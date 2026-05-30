type completed = {
  command : string;
  cwd : string option;
  exit_code : int;
  stdout : string;
  stderr : string;
}

let shell_quote value =
  if value = "" then "''"
  else
    let escaped = String.split_on_char '\'' value |> String.concat "'\\''" in
    "'" ^ escaped ^ "'"

let command_to_string args = String.concat " " (List.map shell_quote args)

let status_to_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED signal -> 128 + signal

let run_shell ?cwd command =
  let stdout_file = Filename.temp_file "mww-stdout-" ".log" in
  let stderr_file = Filename.temp_file "mww-stderr-" ".log" in
  let full_command =
    let body =
      match cwd with
      | None -> command
      | Some dir -> "(cd " ^ shell_quote dir ^ " && " ^ command ^ ")"
    in
    body ^ " > " ^ shell_quote stdout_file ^ " 2> " ^ shell_quote stderr_file
  in
  let cleanup () =
    ignore (Fs.remove_file_if_exists stdout_file : unit Resultx.t);
    ignore (Fs.remove_file_if_exists stderr_file : unit Resultx.t)
  in
  Fun.protect ~finally:cleanup (fun () ->
      let status = Unix.system full_command in
      let exit_code = status_to_code status in
      let stdout = match Fs.read_file stdout_file with Ok value -> value | Error _ -> "" in
      let stderr = match Fs.read_file stderr_file with Ok value -> value | Error _ -> "" in
      Ok { command = full_command; cwd; exit_code; stdout; stderr })

let run_args ?cwd args = run_shell ?cwd (command_to_string args)

let require_success completed =
  if completed.exit_code = 0 then Ok completed
  else
    Error
      (Printf.sprintf "command failed with exit code %d: %s\n%s" completed.exit_code
         completed.command completed.stderr)

let output_trimmed completed = String.trim completed.stdout

let completed_to_yojson c =
  `Assoc
    [
      ("command", `String c.command);
      ("cwd", match c.cwd with Some cwd -> `String cwd | None -> `Null);
      ("exit_code", `Int c.exit_code);
      ("stdout", `String c.stdout);
      ("stderr", `String c.stderr);
    ]
