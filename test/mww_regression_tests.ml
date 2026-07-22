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

let string_contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop index =
      index + needle_len <= haystack_len
      && (String.sub haystack index needle_len = needle || loop (index + 1))
    in
    loop 0

let assert_contains label haystack needle =
  if not (string_contains haystack needle) then
    fail
      (Printf.sprintf "%s\nexpected to find:\n%s\nin:\n%s" label needle haystack)

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

let bool_field name json =
  match member name json with
  | Some (`Bool value) -> value
  | _ -> fail ("missing bool field: " ^ name)

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

let response_data json =
  match member "data" json with Some data -> data | None -> fail "missing response data"

let string_list_field name json =
  list_field name json
  |> List.map (function `String value -> value | _ -> fail ("non-string value in " ^ name))

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

let test_ws_clean_dry_run_preserves_workspace vc_path =
  with_temp_dir (fun root ->
      let app_remote = create_repo root "app" in
      let api_remote = create_repo root "api" in
      let mww_root = root / "mww" in
      init_mww_root vc_path mww_root;
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "app"; app_remote ]);
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; "api"; api_remote ]);
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "new"; "feat"; "app"; "api" ]);
      let workspace_json = load_json (mww_root / "workspaces" / "feat" / ".mww-workspace.json") in
      let app_worktree = string_field "worktree_path" (workspace_repo workspace_json "app") in
      let api_worktree = string_field "worktree_path" (workspace_repo workspace_json "api") in
      let workspace_root = Filename.dirname app_worktree in
      let workspace_meta = workspace_root / ".mww-workspace.json" in
      let ai_context = workspace_root / "AI_CONTEXT.md" in
      let code_workspace = workspace_root / "feat.code-workspace" in
      let metadata_before = read_file workspace_meta in
      let ai_context_before = read_file ai_context in
      let code_workspace_before = read_file code_workspace in
      let assert_workspace_unchanged label =
        assert_bool (label ^ ": app worktree should still exist") (Sys.file_exists app_worktree);
        assert_bool (label ^ ": api worktree should still exist") (Sys.file_exists api_worktree);
        assert_equal_string (label ^ ": metadata should not change") metadata_before
          (read_file workspace_meta);
        assert_equal_string (label ^ ": AI_CONTEXT.md should not change") ai_context_before
          (read_file ai_context);
        assert_equal_string (label ^ ": code workspace should not change") code_workspace_before
          (read_file code_workspace)
      in
      let human =
        vc vc_path ~cwd:mww_root [ "mww"; "ws"; "clean"; "--dry-run"; "feat" ]
      in
      assert_contains "human dry-run should identify itself" human.stdout "Dry run";
      assert_contains "human dry-run should list app worktree" human.stdout app_worktree;
      assert_contains "human dry-run should list api worktree" human.stdout api_worktree;
      assert_contains "human dry-run should list workspace metadata" human.stdout workspace_meta;
      assert_contains "human dry-run should list AI_CONTEXT.md" human.stdout ai_context;
      assert_contains "human dry-run should list code workspace" human.stdout code_workspace;
      assert_workspace_unchanged "human dry-run";
      let json_completed =
        vc vc_path ~cwd:mww_root [ "mww"; "ws"; "clean"; "--dry-run"; "--json"; "feat" ]
      in
      let data = Yojson.Safe.from_string json_completed.stdout |> response_data in
      assert_bool "json dry-run should mark dry_run=true" (bool_field "dry_run" data);
      assert_equal_string "json dry-run should include workspace id" "feat"
        (string_field "workspace_id" data);
      let repo_worktrees =
        list_field "repos" data |> List.map (fun repo -> string_field "worktree_path" repo)
      in
      assert_bool "json dry-run should include app worktree"
        (List.mem app_worktree repo_worktrees);
      assert_bool "json dry-run should include api worktree"
        (List.mem api_worktree repo_worktrees);
      let workspace_files = string_list_field "workspace_files" data in
      assert_bool "json dry-run should include workspace metadata"
        (List.mem workspace_meta workspace_files);
      assert_bool "json dry-run should include AI_CONTEXT.md" (List.mem ai_context workspace_files);
      assert_bool "json dry-run should include code workspace"
        (List.mem code_workspace workspace_files);
      assert_workspace_unchanged "json dry-run";
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "clean"; "feat" ]);
      assert_bool "clean should remove app worktree" (not (Sys.file_exists app_worktree));
      assert_bool "clean should remove api worktree" (not (Sys.file_exists api_worktree));
      assert_bool "clean should remove workspace metadata" (not (Sys.file_exists workspace_meta));
      assert_bool "clean should remove AI_CONTEXT.md" (not (Sys.file_exists ai_context));
      assert_bool "clean should remove code workspace" (not (Sys.file_exists code_workspace)))

let test_ws_clean_retry_after_partial_failure vc_path =
  with_temp_dir (fun root ->
      let repo_names = [ "a"; "b"; "c" ] in
      let remotes = List.map (fun name -> (name, create_repo root name)) repo_names in
      let mww_root = root / "mww" in
      init_mww_root vc_path mww_root;
      List.iter
        (fun (name, remote) ->
          ignore (vc vc_path ~cwd:mww_root [ "mww"; "repo"; "add"; name; remote ]))
        remotes;
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "new"; "feat"; "a"; "b"; "c" ]);
      let workspace_root = mww_root / "workspaces" / "feat" in
      let workspace_meta = workspace_root / ".mww-workspace.json" in
      let ai_context = workspace_root / "AI_CONTEXT.md" in
      let code_workspace = workspace_root / "feat.code-workspace" in
      let workspace_json = load_json workspace_meta in
      let repos =
        List.map
          (fun name ->
            let repo = workspace_repo workspace_json name in
            ( name,
              string_field "source_path" repo,
              string_field "worktree_path" repo,
              string_field "branch" repo ))
          repo_names
      in
      let _, _, b_worktree, _ = List.find (fun (name, _, _, _) -> name = "b") repos in
      write_file (b_worktree / "dirty.txt") "dirty\n";
      let first_clean =
        vc_expect_failure vc_path ~cwd:mww_root [ "mww"; "ws"; "clean"; "feat" ]
      in
      let _, _, a_worktree, _ = List.find (fun (name, _, _, _) -> name = "a") repos in
      let _, _, c_worktree, _ = List.find (fun (name, _, _, _) -> name = "c") repos in
      assert_bool "first clean should remove a" (not (Sys.file_exists a_worktree));
      assert_bool "first clean should stop at dirty b" (Sys.file_exists b_worktree);
      assert_bool "first clean should leave c untouched" (Sys.file_exists c_worktree);
      assert_bool "failed clean should preserve metadata" (Sys.file_exists workspace_meta);
      assert_bool "failed clean should preserve AI_CONTEXT.md" (Sys.file_exists ai_context);
      assert_bool "failed clean should preserve code workspace" (Sys.file_exists code_workspace);
      assert_contains "failed clean should list repos removed before failure" first_clean.stderr
        "removed before failure: a";
      ignore (vc vc_path ~cwd:mww_root [ "mww"; "ws"; "clean"; "--force"; "feat" ]);
      List.iter
        (fun (name, source_path, worktree_path, branch) ->
          assert_bool (name ^ " worktree should be removed") (not (Sys.file_exists worktree_path));
          ignore
            (expect_success ~cwd:source_path
               [ "git"; "show-ref"; "--verify"; "refs/heads/" ^ branch ]);
          let worktrees =
            expect_success ~cwd:source_path [ "git"; "worktree"; "list"; "--porcelain" ]
          in
          assert_bool (name ^ " worktree registry should be clean")
            (not (string_contains worktrees.stdout worktree_path)))
        repos;
      assert_bool "successful retry should remove metadata" (not (Sys.file_exists workspace_meta));
      assert_bool "successful retry should remove AI_CONTEXT.md" (not (Sys.file_exists ai_context));
      assert_bool "successful retry should remove code workspace"
        (not (Sys.file_exists code_workspace)))

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
          test_ws_add_rollback_and_ai_context vc_path);
      run_test "ws clean dry-run preserves workspace" (fun () ->
          test_ws_clean_dry_run_preserves_workspace vc_path);
      run_test "ws clean retry after partial failure" (fun () ->
          test_ws_clean_retry_after_partial_failure vc_path)
  | _ -> fail "usage: mww_regression_tests <path-to-vc>"
