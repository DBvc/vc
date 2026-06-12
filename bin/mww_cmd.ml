open Cmdliner
open Mww
open Resultx

let config_opt =
  let doc = "Path to mww.json. Defaults to the nearest mww.json found by walking up from cwd." in
  Arg.(value & opt (some string) None & info [ "c"; "config" ] ~docv:"FILE" ~doc)

let json_flag =
  let doc = "Print machine-readable JSON output." in
  Arg.(value & flag & info [ "json" ] ~doc)

let repo_filter_opt =
  let doc = "Limit the command to a repo in the workspace. Can be repeated." in
  Arg.(value & opt_all string [] & info [ "r"; "repo" ] ~docv:"REPO" ~doc)

let args_pos docv doc = Arg.(value & pos_all string [] & info [] ~docv ~doc)

let exit_of_result ~json ~to_json ~to_human result =
  match result with
  | Ok value ->
      if json then Render.print_ok_json (to_json value) else to_human value;
      0
  | Error message ->
      Render.print_error ~json message;
      1

let parse_exact label expected args =
  if List.length args = expected then Ok args
  else Error (Printf.sprintf "%s expects %d argument(s)" label expected)

let parse_required_workspace_arg args =
  match args with
  | [ id ] -> Ok id
  | [] -> Error "workspace id is required"
  | _ -> Error "too many positional arguments"

let parse_optional_workspace_arg args =
  match args with
  | [] -> Ok None
  | [ id ] -> Ok (Some id)
  | _ -> Error "too many positional arguments"

let command_result_json results =
  let failed = List.filter (fun r -> r.Types.exit_code <> 0) results |> List.length in
  `Assoc [ ("failed", `Int failed); ("results", Render.command_results_to_yojson results) ]

let print_command_results ~json results =
  let failed = List.filter (fun r -> r.Types.exit_code <> 0) results |> List.length in
  if json then
    Render.print_json
      (Render.json_response ~data:(command_result_json results)
         ?error:
           (if failed = 0 then None else Some (Printf.sprintf "%d repo command(s) failed" failed))
         (failed = 0))
  else Render.print_command_results results;
  if failed = 0 then 0 else 1

let cmd_init config_path json root =
  let result = Config.init ?config_path ~root () in
  exit_of_result ~json
    ~to_json:(fun loaded ->
      `Assoc
        [
          ("config_path", `String loaded.Config.path);
          ("root_dir", `String loaded.root_dir);
          ("config", Types.config_to_yojson loaded.config);
        ])
    ~to_human:(fun loaded ->
      Printf.printf "Initialized mww workspace root:\n  %s\nConfig:\n  %s\n" loaded.root_dir
        loaded.path)
    result

let cmd_repo_add config_path json no_clone args source_path base target disabled =
  let enabled = not disabled in
  let result =
    let* args = parse_exact "repo add" 2 args in
    match args with
    | [ name; url ] ->
        let* loaded = Config.load ?config_path () in
        let repo =
          { Types.name; url; source_path; default_base = base; target_branch = target; enabled }
        in
        let source = Config.repo_source_path loaded repo in
        let* () =
          if no_clone then Ok () else Workspace.ensure_source_clone loaded repo |> Result.map ignore
        in
        let* loaded = Config.upsert_repo loaded repo in
        Ok (loaded, repo, source)
    | _ -> Error "unreachable"
  in
  exit_of_result ~json
    ~to_json:(fun (loaded, repo, source) ->
      `Assoc
        [
          ("config_path", `String loaded.Config.path);
          ("repo", Types.repo_to_yojson repo);
          ("source_path", `String source);
        ])
    ~to_human:(fun (_, repo, source) ->
      Printf.printf "Added repo %s\nSource: %s\n" repo.Types.name source)
    result

let cmd_repo_list config_path json =
  let result =
    let* loaded = Config.load ?config_path () in
    let config = loaded.Config.config in
    Ok config.Types.repos
  in
  exit_of_result ~json
    ~to_json:(fun repos -> `List (List.map Types.repo_to_yojson repos))
    ~to_human:Render.print_repo_list result

let cmd_ws_new config_path json title base branch_template args =
  let result =
    match args with
    | [] -> Error "workspace id is required"
    | id :: repo_names ->
        let* loaded = Config.load ?config_path () in
        Workspace.create ~loaded ~id ?title ?base ?branch_template repo_names
  in
  exit_of_result ~json
    ~to_json:(fun (meta_path, workspace) ->
      `Assoc
        [ ("meta_path", `String meta_path); ("workspace", Types.workspace_to_yojson workspace) ])
    ~to_human:(fun (meta_path, workspace) ->
      Printf.printf "Created workspace %s\nMetadata: %s\nRepos: %d\n" workspace.Types.id meta_path
        (List.length workspace.repos))
    result

let cmd_ws_list config_path json =
  let result =
    let* loaded = Config.load ?config_path () in
    Config.list_workspaces loaded
  in
  exit_of_result ~json
    ~to_json:(fun workspaces -> `List (List.map Render.workspace_summary_to_yojson workspaces))
    ~to_human:Render.print_workspace_list result

let load_workspace_for_cmd config_path workspace_id =
  let* loaded = Config.load ?config_path () in
  let* workspace_path, workspace = Config.load_workspace ?id:workspace_id loaded in
  Ok (loaded, workspace_path, workspace)

let cmd_ws_add config_path json base branch_template args =
  let result =
    match args with
    | [] -> Error "workspace id is required"
    | [ _ ] -> Error "repo name is required"
    | id :: repo_names ->
        let* loaded, workspace_path, workspace = load_workspace_for_cmd config_path (Some id) in
        Workspace.add_repos ~loaded ~workspace_path ~workspace ?base ?branch_template repo_names
  in
  exit_of_result ~json
    ~to_json:(fun (meta_path, workspace) ->
      `Assoc
        [ ("meta_path", `String meta_path); ("workspace", Types.workspace_to_yojson workspace) ])
    ~to_human:(fun (meta_path, workspace) ->
      Printf.printf "Updated workspace %s\nMetadata: %s\nRepos: %d\n" workspace.Types.id meta_path
        (List.length workspace.repos))
    result

let cmd_ws_status config_path json repo_filters args =
  let result =
    let* workspace_id = parse_optional_workspace_arg args in
    let* _loaded, _workspace_path, workspace = load_workspace_for_cmd config_path workspace_id in
    let* statuses = Workspace.status ~workspace ~repo_filters in
    Ok (workspace, statuses)
  in
  exit_of_result ~json
    ~to_json:(fun (workspace, statuses) ->
      `Assoc
        [
          ("workspace", Types.workspace_to_yojson workspace);
          ("statuses", `List (List.map Types.repo_status_to_yojson statuses));
        ])
    ~to_human:(fun (workspace, statuses) -> Render.print_status workspace statuses)
    result

let cmd_ws_clean config_path json force args =
  let result =
    let* id = parse_required_workspace_arg args in
    let* _loaded, workspace_path, workspace = load_workspace_for_cmd config_path (Some id) in
    let* results = Workspace.clean ~workspace ~force () in
    let workspace_root = Filename.dirname workspace_path in
    let* () = Fs.remove_file_if_exists workspace_path in
    let* () = Fs.remove_file_if_exists (Filename.concat workspace_root "AI_CONTEXT.md") in
    let* () =
      Fs.remove_file_if_exists
        (Filename.concat workspace_root (workspace.Types.id ^ ".code-workspace"))
    in
    let* () = Fs.rmdir_if_empty workspace_root in
    Ok results
  in
  match result with
  | Ok results -> print_command_results ~json results
  | Error message ->
      Render.print_error ~json message;
      1

let workspace_exists loaded id = Fs.path_exists (Config.workspace_path loaded id)

let parse_run_args loaded args =
  match args with
  | [] -> Error "command is required"
  | first :: rest when rest <> [] && workspace_exists loaded first ->
      Ok (Some first, String.concat " " rest)
  | _ -> Ok (None, String.concat " " args)

let cmd_run config_path json repo_filters args =
  let result =
    let* loaded = Config.load ?config_path () in
    let* workspace_id, command = parse_run_args loaded args in
    let* _workspace_path, workspace = Config.load_workspace ?id:workspace_id loaded in
    Workspace.run_in_repos ~workspace ~repo_filters ~command
  in
  match result with
  | Ok results -> print_command_results ~json results
  | Error message ->
      Render.print_error ~json message;
      1

let cmd_push config_path json repo_filters args =
  let result =
    let* workspace_id = parse_optional_workspace_arg args in
    let* _loaded, _workspace_path, workspace = load_workspace_for_cmd config_path workspace_id in
    Workspace.push ~workspace ~repo_filters
  in
  match result with
  | Ok results -> print_command_results ~json results
  | Error message ->
      Render.print_error ~json message;
      1

let cmd_mr_create config_path json repo_filters target_override args =
  let result =
    let* workspace_id = parse_optional_workspace_arg args in
    let* loaded, workspace_path, workspace = load_workspace_for_cmd config_path workspace_id in
    let* results, updated_workspace =
      Workspace.mr_create ~loaded ~workspace_path ~workspace ~repo_filters ?target_override ()
    in
    Ok (results, updated_workspace)
  in
  match result with
  | Ok (results, updated_workspace) ->
      let failed = List.filter (fun r -> r.Types.exit_code <> 0) results |> List.length in
      if json then
        Render.print_json
          (Render.json_response
             ~data:
               (`Assoc
                  [
                    ("failed", `Int failed);
                    ("workspace", Types.workspace_to_yojson updated_workspace);
                    ("results", Render.command_results_to_yojson results);
                  ])
             ?error:
               (if failed = 0 then None else Some (Printf.sprintf "%d MR command(s) failed" failed))
             (failed = 0))
      else Render.print_command_results results;
      if failed = 0 then 0 else 1
  | Error message ->
      Render.print_error ~json message;
      1

let cmd_doctor config_path json =
  let git = Git.version () in
  let glab_tool =
    match Config.load ?config_path () with
    | Ok loaded ->
        let config = loaded.Config.config in
        let gitlab = config.Types.gitlab in
        gitlab.Types.mr_tool
    | Error _ -> "glab"
  in
  let glab = Proc.run_args [ glab_tool; "--version" ] in
  let normalize name = function
    | Ok completed ->
        `Assoc
          [
            ("name", `String name);
            ("ok", `Bool (completed.Proc.exit_code = 0));
            ("exit_code", `Int completed.exit_code);
            ("stdout", `String completed.stdout);
            ("stderr", `String completed.stderr);
          ]
    | Error message ->
        `Assoc [ ("name", `String name); ("ok", `Bool false); ("error", `String message) ]
  in
  let data = `List [ normalize "git" git; normalize glab_tool glab ] in
  let git_error =
    match git with
    | Ok completed when completed.Proc.exit_code = 0 -> None
    | Ok completed ->
        Some (Printf.sprintf "required tool git failed with exit code %d" completed.exit_code)
    | Error message -> Some ("required tool git failed: " ^ message)
  in
  match git_error with
  | None ->
      if json then Render.print_ok_json data else print_endline (Yojson.Safe.pretty_to_string data);
      0
  | Some message ->
      if json then Render.print_json (Render.json_response ~data ~error:message false)
      else (
        print_endline (Yojson.Safe.pretty_to_string data);
        Render.print_error message);
      1

let init_cmd =
  let root_arg =
    let doc = "Root directory for the local multi-repo workspace." in
    Arg.(value & pos 0 string "." & info [] ~docv:"ROOT" ~doc)
  in
  Cmd.v
    (Cmd.info "init" ~doc:"Initialize an mww.json config file")
    Term.(const cmd_init $ config_opt $ json_flag $ root_arg)

let repo_add_cmd =
  let no_clone =
    let doc = "Only write config; do not clone the repository into repos/." in
    Arg.(value & flag & info [ "no-clone" ] ~doc)
  in
  let source_path =
    let doc = "Custom local source clone path. Relative paths are resolved from workspace root." in
    Arg.(value & opt (some string) None & info [ "path" ] ~docv:"PATH" ~doc)
  in
  let base =
    let doc = "Default base revision for worktree creation, e.g. origin/main." in
    Arg.(value & opt (some string) None & info [ "base" ] ~docv:"REV" ~doc)
  in
  let target =
    let doc = "Default GitLab target branch for MRs." in
    Arg.(value & opt (some string) None & info [ "target" ] ~docv:"BRANCH" ~doc)
  in
  let disabled =
    let doc = "Add the repo but do not include it when ws new is called without repo names." in
    Arg.(value & flag & info [ "disabled" ] ~doc)
  in
  let args = args_pos "NAME URL" "Repository name and Git remote URL." in
  Cmd.v
    (Cmd.info "add" ~doc:"Add or update a repository in mww.json")
    Term.(
      const cmd_repo_add $ config_opt $ json_flag $ no_clone $ args $ source_path $ base $ target
      $ disabled)

let repo_list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"List configured repositories")
    Term.(const cmd_repo_list $ config_opt $ json_flag)

let repo_cmd =
  Cmd.group (Cmd.info "repo" ~doc:"Manage repository registry") [ repo_add_cmd; repo_list_cmd ]

let ws_new_cmd =
  let title =
    let doc = "Human-readable workspace title." in
    Arg.(value & opt (some string) None & info [ "title" ] ~docv:"TITLE" ~doc)
  in
  let base =
    let doc = "Override base revision for all repos in this workspace." in
    Arg.(value & opt (some string) None & info [ "base" ] ~docv:"REV" ~doc)
  in
  let branch_template =
    let doc = "Override branch template. Variables: {user}, {workspace}, {repo}." in
    Arg.(value & opt (some string) None & info [ "branch-template" ] ~docv:"TEMPLATE" ~doc)
  in
  let args = args_pos "WORKSPACE [REPO ...]" "Workspace id followed by optional repo names." in
  Cmd.v
    (Cmd.info "new" ~doc:"Create a feature workspace with Git worktrees")
    Term.(const cmd_ws_new $ config_opt $ json_flag $ title $ base $ branch_template $ args)

let ws_add_cmd =
  let base =
    let doc = "Override base revision for added repos in this workspace." in
    Arg.(value & opt (some string) None & info [ "base" ] ~docv:"REV" ~doc)
  in
  let branch_template =
    let doc = "Override branch template for added repos. Variables: {user}, {workspace}, {repo}." in
    Arg.(value & opt (some string) None & info [ "branch-template" ] ~docv:"TEMPLATE" ~doc)
  in
  let args = args_pos "WORKSPACE REPO..." "Workspace id followed by one or more repo names." in
  Cmd.v
    (Cmd.info "add" ~doc:"Add repositories to an existing feature workspace")
    Term.(const cmd_ws_add $ config_opt $ json_flag $ base $ branch_template $ args)

let ws_list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"List local workspaces")
    Term.(const cmd_ws_list $ config_opt $ json_flag)

let ws_status_cmd =
  let args = args_pos "[WORKSPACE]" "Workspace id. Omit it inside a workspace directory." in
  Cmd.v
    (Cmd.info "status" ~doc:"Show Git status for repos in a workspace")
    Term.(const cmd_ws_status $ config_opt $ json_flag $ repo_filter_opt $ args)

let ws_clean_cmd =
  let force =
    let doc = "Pass --force to git worktree remove." in
    Arg.(value & flag & info [ "f"; "force" ] ~doc)
  in
  let args = args_pos "WORKSPACE" "Workspace id to remove." in
  Cmd.v
    (Cmd.info "clean" ~doc:"Remove worktrees and workspace metadata")
    Term.(const cmd_ws_clean $ config_opt $ json_flag $ force $ args)

let ws_cmd =
  Cmd.group
    (Cmd.info "ws" ~doc:"Manage feature workspaces")
    [ ws_new_cmd; ws_add_cmd; ws_list_cmd; ws_status_cmd; ws_clean_cmd ]

let run_cmd =
  let args =
    args_pos "[WORKSPACE] -- COMMAND ..."
      "Command to run. Workspace is optional if cwd is inside one."
  in
  Cmd.v
    (Cmd.info "run" ~doc:"Run a shell command in each repo worktree")
    Term.(const cmd_run $ config_opt $ json_flag $ repo_filter_opt $ args)

let push_cmd =
  let args = args_pos "[WORKSPACE]" "Workspace id. Omit it inside a workspace directory." in
  Cmd.v
    (Cmd.info "push" ~doc:"Push current branches for repos in a workspace")
    Term.(const cmd_push $ config_opt $ json_flag $ repo_filter_opt $ args)

let mr_create_cmd =
  let target =
    let doc = "Override GitLab target branch for all MRs." in
    Arg.(value & opt (some string) None & info [ "target" ] ~docv:"BRANCH" ~doc)
  in
  let args = args_pos "[WORKSPACE]" "Workspace id. Omit it inside a workspace directory." in
  Cmd.v
    (Cmd.info "create" ~doc:"Create GitLab merge requests with glab")
    Term.(const cmd_mr_create $ config_opt $ json_flag $ repo_filter_opt $ target $ args)

let mr_cmd = Cmd.group (Cmd.info "mr" ~doc:"Manage GitLab merge requests") [ mr_create_cmd ]

let doctor_cmd =
  Cmd.v
    (Cmd.info "doctor" ~doc:"Check external tool availability")
    Term.(const cmd_doctor $ config_opt $ json_flag)

let cmd =
  let doc = "Local virtual-monorepo manager for multi-repo Git worktree workflows." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "mww turns a multi-repository feature into a local workspace: one task directory, many Git \
         worktrees, independent GitLab merge requests.";
      `S Manpage.s_examples;
      `P "vc mww init ~/dev/company";
      `P "vc mww repo add frontend git@gitlab.example.com:team/frontend.git";
      `P "vc mww ws new FEAT-123-login frontend backend";
      `P "vc mww ws add FEAT-123-login api";
      `P "vc mww ws status FEAT-123-login --json";
      `P "vc mww run FEAT-123-login -- pnpm test";
    ]
  in
  Cmd.group
    (Cmd.info "mww" ~version:"0.1.0" ~doc ~man)
    [ init_cmd; repo_cmd; ws_cmd; run_cmd; push_cmd; mr_cmd; doctor_cmd ]
