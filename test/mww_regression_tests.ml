type completed = {
  command : string;
  exit_code : int;
  stdout : string;
  stderr : string;
}

let ( / ) = Filename.concat

let fail message = failwith message

let assert_bool message condition = if not condition then fail message

let assert_equal_string label expected actual =
  if expected <> actual then
    fail
      (Printf.sprintf "%s\nexpected:\n%s\nactual:\n%s" label expected actual)

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

let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let write_file path contents =
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch contents)

let run ?cwd args =
  let stdout_file = Filename.temp_file "vc-mww-test-stdout-" ".log" in
  let stderr_file = Filename.temp_file "vc-mww-test-stderr-" ".log" in
  let body =
    match cwd with
    | None -> command_to_string args
    | Some dir -> "(cd " ^ shell_quote dir ^ " && " ^ command_to_string args ^ ")"
  in
  let command = body ^ " > " ^ shell_quote stdout_file ^ " 2> " ^ shell_quote stderr_file in
  let cleanup () =
    (try Sys.remove stdout_file with Sys_error _ -> ());
    try Sys.remove stderr_file with Sys_error _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      let exit_code = Unix.system command |> status_to_code in
      let stdout = try read_file stdout_file with Sys_error _ -> "" in
      let stderr = try read_file stderr_file with Sys_error _ -> "" in
      { command; exit_code; stdout; stderr })

let expect_success ?cwd args =
  let completed = run ?cwd args in
  if completed.exit_code <> 0 then
    fail
      (Printf.sprintf "command failed: %s\nstdout:\n%s\nstderr:\n%s" completed.command
         completed.stdout completed.stderr);
  completed

let expect_failure ?cwd args =
  let completed = run ?cwd args in
  if completed.exit_code = 0 then
    fail
      (Printf.sprintf "command unexpectedly succeeded: %s\nstdout:\n%s\nstderr:\n%s"
         completed.command completed.stdout completed.stderr);
  completed

let mkdir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

let rec mkdir_p path =
  if path = "" || path = "." then ()
  else if Sys.file_exists path then
    assert_bool (path ^ " exists but is not a directory") (Sys.is_directory path)
  else (
    mkdir_p (Filename.dirname path);
    mkdir path)

let create_temp_dir () =
  let path = Filename.temp_file "vc-mww-test-" "" in
  Sys.remove path;
  mkdir path;
  path

let remove_tree root =
  let base = Filename.basename root in
  assert_bool
    ("refusing to remove non-test directory: " ^ root)
    (String.starts_with ~prefix:"vc-mww-test-" base);
  let rec loop path =
    match Unix.lstat path with
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Sys.readdir path
        |> Array.iter (fun name -> loop (path / name));
        Unix.rmdir path
    | _ -> Sys.remove path
    | exception Unix.Unix_error _ -> ()
  in
  loop root

let with_temp_dir f =
  let root = create_temp_dir () in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () -> f root)

let git ?cwd args = ignore (expect_success ?cwd ("git" :: args))

let create_repo root name =
  let path = root / "remotes" / name in
  mkdir_p path;
  git [ "init"; "--initial-branch=master"; path ];
  git ~cwd:path [ "config"; "user.email"; "mww-test@example.com" ];
  git ~cwd:path [ "config"; "user.name"; "Mww Test" ];
  git ~cwd:path [ "config"; "commit.gpgsign"; "false" ];
  write_file (path / "README.md") ("# " ^ name ^ "\n");
  git ~cwd:path [ "add"; "README.md" ];
  git ~cwd:path [ "commit"; "-m"; "init" ];
  path

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match member name json with
  | Some (`String value) -> value
  | _ -> fail ("missing string field: " ^ name)

let list_field name json =
  match member name json with
  | Some (`List values) -> values
  | _ -> fail ("missing list field: " ^ name)

let workspace_repo workspace_json repo_name =
  workspace_json |> list_field "repos"
  |> List.find_opt (fun repo -> string_field "name" repo = repo_name)
  |> function
  | Some repo -> repo
  | None -> fail ("repo not found in workspace metadata: " ^ repo_name)

let load_json path = Yojson.Safe.from_string (read_file path)

let write_json path json =
  write_file path (Yojson.Safe.pretty_to_string json ^ "\n")

let set_string_field name value = function
  | `Assoc fields ->
      let fields =
        (name, `String value)
        :: List.filter (fun (field_name, _) -> field_name <> name) fields
      in
      `Assoc fields
  | _ -> fail "expected JSON object"

let vc vc_path ?cwd args = expect_success ?cwd (vc_path :: args)

let vc_expect_failure vc_path ?cwd args = expect_failure ?cwd (vc_path :: args)

let init_mww_root vc_path root =
  ignore (vc vc_path [ "mww"; "init"; root ])

let test_master_only_remote_fallback vc_path =
  with_temp_dir (fun root ->
      let remote = create_repo root "app" in
      let mww_root = root / "mww" in
      init_mww_root vc_path mww_root;
      let config_path = mww_root / "mww.json" in
      load_json config_path |> set_string_field "default_base" "origin/main"
      |> write_json config_path;
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "app"; remote ]);
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "new"; "feat"; "app" ]);
      let workspace_json = load_json (mww_root / "workspaces" / "feat" / ".mww-workspace.json") in
      let app_repo = workspace_repo workspace_json "app" in
      assert_equal_string "master-only repo should fall back to origin/master" "origin/master"
        (string_field "base" app_repo);
      assert_equal_string "target branch should follow remote default branch" "master"
        (string_field "target_branch" app_repo);
      assert_bool "app worktree should exist" (Sys.file_exists (mww_root / "workspaces" / "feat" / "app")))

let test_ws_add_rollback_and_ai_context vc_path =
  with_temp_dir (fun root ->
      let app_remote = create_repo root "app" in
      let api_remote = create_repo root "api" in
      let extra_remote = create_repo root "extra" in
      let missing_remote = root / "remotes" / "missing" in
      let mww_root = root / "mww" in
      init_mww_root vc_path mww_root;
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "app"; app_remote ]);
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "new"; "feat"; "app" ]);
      let workspace_root = mww_root / "workspaces" / "feat" in
      let workspace_meta = workspace_root / ".mww-workspace.json" in
      let ai_context = workspace_root / "AI_CONTEXT.md" in
      let custom_context = "custom AI context\nuser-owned notes stay intact\n" in
      write_file ai_context custom_context;
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "api"; api_remote ]);
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "add"; "feat"; "api" ]);
      assert_equal_string "successful ws add must not rewrite AI_CONTEXT.md" custom_context
        (read_file ai_context);
      let metadata_before_failure = read_file workspace_meta in
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "extra"; extra_remote ]);
      ignore
        (vc vc_path ~cwd:mww_root
           [ "mww"; "repo"; "add"; "--no-clone"; "broken"; missing_remote ]);
      ignore (vc_expect_failure vc_path ~cwd:mww_root [ "mww"; "ws"; "add"; "feat"; "extra"; "broken" ]);
      assert_equal_string "failed ws add should restore metadata" metadata_before_failure
        (read_file workspace_meta);
      assert_equal_string "failed ws add must not rewrite AI_CONTEXT.md" custom_context
        (read_file ai_context);
      assert_bool "failed ws add should remove worktree created in this batch"
        (not (Sys.file_exists (workspace_root / "extra"))))

let run_test name f =
  try
    f ();
    Printf.printf "ok - %s\n%!" name
  with exn ->
    Printf.eprintf "not ok - %s\n%s\n%!" name (Printexc.to_string exn);
    exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _; vc_path ] ->
      let vc_path =
        if Filename.is_relative vc_path then Filename.concat (Sys.getcwd ()) vc_path else vc_path
      in
      run_test "master-only remote base fallback" (fun () ->
          test_master_only_remote_fallback vc_path);
      run_test "ws add rollback and AI_CONTEXT preservation" (fun () ->
          test_ws_add_rollback_and_ai_context vc_path)
  | _ -> fail "usage: mww_regression_tests <path-to-vc>"
