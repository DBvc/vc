open Resultx

let iso_now () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let selected_repos (loaded : Config.loaded) names =
  let all = loaded.Config.config.Types.repos in
  if names = [] then Ok (List.filter (fun (repo : Types.repo) -> repo.Types.enabled) all)
  else
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | name :: rest -> (
          match Config.find_repo loaded name with
          | Some repo -> loop (repo :: acc) rest
          | None -> Error ("unknown repo: " ^ name))
    in
    loop [] names

let duplicate_name names =
  let rec loop seen = function
    | [] -> None
    | name :: rest ->
        if List.mem name seen then Some name else loop (name :: seen) rest
  in
  loop [] names

let render_branch_template ~template ~workspace ~repo =
  template
  |> Text.replace_all ~pattern:"{user}" ~with_:(Fs.current_user ())
  |> Text.replace_all ~pattern:"{workspace}" ~with_:workspace
  |> Text.replace_all ~pattern:"{repo}" ~with_:repo

type base_choice = { value : string; explicit : bool }

let repo_base (loaded : Config.loaded) (repo : Types.repo) override_base =
  match override_base with
  | Some base -> { value = base; explicit = true }
  | None -> (
      match repo.Types.default_base with
      | Some base -> { value = base; explicit = true }
      | None -> { value = loaded.Config.config.default_base; explicit = false })

let branch_of_origin_ref ref =
  let prefix = "origin/" in
  let prefix_len = String.length prefix in
  if String.length ref > prefix_len && String.sub ref 0 prefix_len = prefix then
    let branch = String.sub ref prefix_len (String.length ref - prefix_len) in
    if branch = "HEAD" then None else Some branch
  else None

let rec first_existing_ref ~repo_path = function
  | [] -> Ok None
  | ref :: rest ->
      let* exists = Git.ref_exists ~cwd:repo_path ref in
      if exists then Ok (Some ref) else first_existing_ref ~repo_path rest

let resolve_repo_base ~repo_path ~repo_name (base : base_choice) ~remote_default_ref =
  let* exists = Git.ref_exists ~cwd:repo_path base.value in
  if exists then Ok base.value
  else if base.explicit then
    Error
      (Printf.sprintf "base %s does not exist for repo %s; pass --base or update repo config"
         base.value repo_name)
  else
    let candidates =
      [ remote_default_ref; Some "origin/master"; Some "origin/main" ]
      |> List.filter_map Fun.id
      |> List.filter (fun ref -> ref <> base.value)
    in
    let* fallback = first_existing_ref ~repo_path candidates in
    match fallback with
    | Some ref -> Ok ref
    | None ->
        Error
          (Printf.sprintf
             "default base %s does not exist for repo %s and no remote default branch was found"
             base.value repo_name)

let repo_target_branch (loaded : Config.loaded) (repo : Types.repo) ~base ~remote_default_branch =
  match repo.Types.target_branch with
  | Some branch -> branch
  | None ->
      let configured = loaded.Config.config.default_target_branch in
      if configured <> "main" then configured
      else
        match remote_default_branch with
        | Some branch -> branch
        | None -> (
            match branch_of_origin_ref base with Some branch -> branch | None -> configured)

let ensure_source_clone (loaded : Config.loaded) (repo : Types.repo) =
  let source_path = Config.repo_source_path loaded repo in
  if Git.has_git_dir source_path then Ok source_path
  else
    let* cloned = Git.clone ~url:repo.Types.url ~dest:source_path in
    ignore cloned;
    Ok source_path

let create_workspace_repo ~(loaded : Config.loaded) ~workspace_id ~workspace_root ~template ?base
    (repo : Types.repo) =
  let* source_path = ensure_source_clone loaded repo in
  let* () = Git.fetch ~repo_path:source_path in
  let* remote_default_ref = Git.remote_default_ref ~repo_path:source_path in
  let remote_default_branch =
    match remote_default_ref with Some ref -> branch_of_origin_ref ref | None -> None
  in
  let branch = render_branch_template ~template ~workspace:workspace_id ~repo:repo.name in
  let worktree_path = Filename.concat workspace_root repo.name in
  let* base =
    resolve_repo_base ~repo_path:source_path ~repo_name:repo.name (repo_base loaded repo base)
      ~remote_default_ref
  in
  let target_branch = repo_target_branch loaded repo ~base ~remote_default_branch in
  let* _ = Git.worktree_add ~repo_path:source_path ~branch ~dest:worktree_path ~base in
  Ok
    {
      Types.name = repo.name;
      url = repo.url;
      source_path;
      worktree_path;
      base;
      branch;
      target_branch;
      mr_url = None;
    }

let write_vscode_workspace workspace_root (workspace : Types.workspace) =
  let folders =
    workspace.Types.repos
    |> List.map (fun (repo : Types.workspace_repo) ->
           `Assoc [ ("name", `String repo.Types.name); ("path", `String ("./" ^ repo.name)) ])
  in
  let json = `Assoc [ ("folders", `List folders); ("settings", `Assoc []) ] in
  let path = Filename.concat workspace_root (workspace.id ^ ".code-workspace") in
  Json_util.write_file path json

let write_ai_context workspace_root (workspace : Types.workspace) =
  let title = match workspace.Types.title with Some value -> value | None -> workspace.id in
  let repo_lines =
    workspace.repos
    |> List.map (fun (repo : Types.workspace_repo) ->
           Printf.sprintf "- %s: `%s`, branch `%s`, target `%s`" repo.Types.name repo.worktree_path
             repo.branch repo.target_branch)
    |> String.concat "\n"
  in
  let contents =
    Printf.sprintf
      {|# %s

## Goal

Describe the task here. Keep this file local to the workspace and update it as the task evolves.

## Repos

%s

## Cross-repo notes

- This workspace is a local virtual monorepo. Each child directory is still an independent Git repository/worktree.
- Push and merge requests go back to the original GitLab projects.
- Prefer small, compatible changes across repos. Document merge order before opening MRs.
|}
      title repo_lines
  in
  Fs.write_file (Filename.concat workspace_root "AI_CONTEXT.md") contents

let rollback_created_workspace workspace_root workspace_id created_repos =
  let errors = ref [] in
  let note_error message = errors := message :: !errors in
  List.iter
    (fun (repo : Types.workspace_repo) ->
      match
        Git.worktree_remove ~force:true ~repo_path:repo.Types.source_path ~dest:repo.worktree_path
          ()
      with
      | Ok _ -> ()
      | Error message -> note_error message)
    created_repos;
  List.iter
    (fun path ->
      match Fs.remove_file_if_exists path with Ok () -> () | Error message -> note_error message)
    [
      Config.workspace_meta_path workspace_root;
      Filename.concat workspace_root "AI_CONTEXT.md";
      Filename.concat workspace_root (workspace_id ^ ".code-workspace");
    ];
  (match Fs.rmdir_if_empty workspace_root with Ok () -> () | Error message -> note_error message);
  match List.rev !errors with [] -> None | errors -> Some (String.concat "; " errors)

let rollback_workspace_repos created_repos =
  let errors = ref [] in
  let note_error message = errors := message :: !errors in
  List.iter
    (fun (repo : Types.workspace_repo) ->
      match
        Git.worktree_remove ~force:true ~repo_path:repo.Types.source_path ~dest:repo.worktree_path
          ()
      with
      | Ok _ -> ()
      | Error message -> note_error message)
    created_repos;
  match List.rev !errors with [] -> None | errors -> Some (String.concat "; " errors)

let create ~(loaded : Config.loaded) ~id ?title ?base ?branch_template repo_names =
  let workspace_root = Config.workspace_root loaded id in
  if Fs.path_exists workspace_root then Error (workspace_root ^ " already exists")
  else
    let created_repos = ref [] in
    let rollback_error message =
      match rollback_created_workspace workspace_root id !created_repos with
      | None -> Error message
      | Some cleanup_error -> Error (message ^ "\nrollback failed: " ^ cleanup_error)
    in
    let result =
      let* repos = selected_repos loaded repo_names in
      if repos = [] then Error "no repos selected; add repos first or pass repo names"
      else
        let template =
          match branch_template with Some value -> value | None -> loaded.config.branch_template
        in
        let* () = Fs.mkdir_p workspace_root in
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | (repo : Types.repo) :: rest ->
              let* workspace_repo =
                create_workspace_repo ~loaded ~workspace_id:id ~workspace_root ~template ?base repo
              in
              created_repos := workspace_repo :: !created_repos;
              loop (workspace_repo :: acc) rest
        in
        let* workspace_repos = loop [] repos in
        let workspace =
          { Types.version = 1; id; title; created_at = iso_now (); repos = workspace_repos }
        in
        let meta_path = Config.workspace_meta_path workspace_root in
        let* () = Config.save_workspace meta_path workspace in
        let* () = write_vscode_workspace workspace_root workspace in
        let* () = write_ai_context workspace_root workspace in
        Ok (meta_path, workspace)
    in
    match result with Ok _ as ok -> ok | Error message -> rollback_error message

let restore_file path = function
  | Some contents -> Fs.write_file path contents
  | None -> Fs.remove_file_if_exists path

let restore_files backups =
  let errors = ref [] in
  let note_error message = errors := message :: !errors in
  List.iter
    (fun (path, contents) ->
      match restore_file path contents with Ok () -> () | Error message -> note_error message)
    backups;
  match List.rev !errors with [] -> None | errors -> Some (String.concat "; " errors)

let read_optional_file path =
  if Fs.path_exists path then Fs.read_file path |> Result.map Option.some else Ok None

let workspace_contains_repo (workspace : Types.workspace) name =
  List.exists (fun (repo : Types.workspace_repo) -> repo.Types.name = name) workspace.repos

let validate_add_repos ~(workspace : Types.workspace) ~workspace_root repos =
  let rec loop = function
    | [] -> Ok ()
    | (repo : Types.repo) :: rest ->
        if workspace_contains_repo workspace repo.name then
          Error (Printf.sprintf "repo already in workspace %s: %s" workspace.id repo.name)
        else
          let worktree_path = Filename.concat workspace_root repo.name in
          if Fs.path_exists worktree_path then Error (worktree_path ^ " already exists")
          else loop rest
  in
  loop repos

let add_repos ~(loaded : Config.loaded) ~workspace_path ~(workspace : Types.workspace) ?base
    ?branch_template repo_names =
  match repo_names with
  | [] -> Error "repo name is required"
  | _ -> (
      match duplicate_name repo_names with
      | Some name -> Error ("duplicate repo: " ^ name)
      | None ->
          let workspace_root = Filename.dirname workspace_path in
          let template =
            match branch_template with Some value -> value | None -> loaded.config.branch_template
          in
          let created_repos = ref [] in
          let backups = ref None in
          let rollback_error message =
            let rollback_message = rollback_workspace_repos !created_repos in
            let restore_message =
              match !backups with Some backups -> restore_files backups | None -> None
            in
            let details =
              [ rollback_message; restore_message ] |> List.filter_map Fun.id
              |> String.concat "; "
            in
            if details = "" then Error message else Error (message ^ "\nrollback failed: " ^ details)
          in
          let result =
            let* repos = selected_repos loaded repo_names in
            let* () = validate_add_repos ~workspace ~workspace_root repos in
            let rec loop acc = function
              | [] -> Ok (List.rev acc)
              | (repo : Types.repo) :: rest ->
                  let* workspace_repo =
                    create_workspace_repo ~loaded ~workspace_id:workspace.id ~workspace_root
                      ~template ?base repo
                  in
                  created_repos := workspace_repo :: !created_repos;
                  loop (workspace_repo :: acc) rest
            in
            let* added_repos = loop [] repos in
            let updated_workspace =
              { workspace with Types.repos = workspace.repos @ added_repos }
            in
            let code_workspace_path =
              Filename.concat workspace_root (workspace.Types.id ^ ".code-workspace")
            in
            let* workspace_backup = Fs.read_file workspace_path in
            let* code_workspace_backup = read_optional_file code_workspace_path in
            backups :=
              Some
                [
                  (workspace_path, Some workspace_backup);
                  (code_workspace_path, code_workspace_backup);
                ];
            let* () = write_vscode_workspace workspace_root updated_workspace in
            let* () = Config.save_workspace workspace_path updated_workspace in
            Ok (workspace_path, updated_workspace)
          in
          match result with Ok _ as ok -> ok | Error message -> rollback_error message)

let filter_workspace_repos (workspace : Types.workspace) repo_filters =
  if repo_filters = [] then Ok workspace.Types.repos
  else
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | name :: rest -> (
          match
            List.find_opt
              (fun (repo : Types.workspace_repo) -> repo.Types.name = name)
              workspace.repos
          with
          | Some repo -> loop (repo :: acc) rest
          | None -> Error ("repo not in workspace " ^ workspace.id ^ ": " ^ name))
    in
    loop [] repo_filters

let status_for_repo (repo : Types.workspace_repo) =
  if not (Fs.path_exists repo.Types.worktree_path) then
    Ok
      {
        Types.name = repo.name;
        path = repo.worktree_path;
        branch = None;
        upstream = None;
        dirty = false;
        dirty_count = 0;
        ahead = None;
        behind = None;
        mr_url = repo.mr_url;
        exists = false;
      }
  else
    let* branch = Git.current_branch ~cwd:repo.worktree_path in
    let* upstream = Git.upstream ~cwd:repo.worktree_path in
    let* status_output = Git.status_porcelain ~cwd:repo.worktree_path in
    let dirty_count = Git.dirty_count status_output in
    let* ahead, behind =
      match upstream with
      | Some upstream -> Git.ahead_behind ~cwd:repo.worktree_path ~upstream
      | None -> Ok (None, None)
    in
    Ok
      {
        Types.name = repo.name;
        path = repo.worktree_path;
        branch;
        upstream;
        dirty = dirty_count > 0;
        dirty_count;
        ahead;
        behind;
        mr_url = repo.mr_url;
        exists = true;
      }

let status ~(workspace : Types.workspace) ~repo_filters =
  let* repos = filter_workspace_repos workspace repo_filters in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (repo : Types.workspace_repo) :: rest ->
        let* status = status_for_repo repo in
        loop (status :: acc) rest
  in
  loop [] repos

let run_in_repos ~(workspace : Types.workspace) ~repo_filters ~command =
  let* repos = filter_workspace_repos workspace repo_filters in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (repo : Types.workspace_repo) :: rest ->
        let* completed = Proc.run_shell ~cwd:repo.Types.worktree_path command in
        let result =
          {
            Types.name = repo.name;
            path = repo.worktree_path;
            command = completed.Proc.command;
            exit_code = completed.exit_code;
            stdout = completed.stdout;
            stderr = completed.stderr;
          }
        in
        loop (result :: acc) rest
  in
  loop [] repos

let push ~(workspace : Types.workspace) ~repo_filters =
  let* repos = filter_workspace_repos workspace repo_filters in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (repo : Types.workspace_repo) :: rest ->
        let* completed = Git.push_current_branch ~cwd:repo.Types.worktree_path in
        let result =
          {
            Types.name = repo.name;
            path = repo.worktree_path;
            command = completed.Proc.command;
            exit_code = completed.exit_code;
            stdout = completed.stdout;
            stderr = completed.stderr;
          }
        in
        loop (result :: acc) rest
  in
  loop [] repos

let mr_create_for_repo ~(gitlab : Types.gitlab_config) ?target_override
    (repo : Types.workspace_repo) =
  let target =
    match target_override with Some value -> value | None -> repo.Types.target_branch
  in
  let args =
    [ gitlab.Types.mr_tool; "mr"; "create"; "--fill"; "--target-branch"; target ]
    @ (if gitlab.yes then [ "--yes" ] else [])
    @ (if gitlab.draft then [ "--draft" ] else [])
    @ (if gitlab.remove_source_branch then [ "--remove-source-branch" ] else [])
    @ List.concat_map (fun label -> [ "--label"; label ]) gitlab.labels
  in
  let* completed = Proc.run_args ~cwd:repo.Types.worktree_path args in
  let output = completed.stdout ^ "\n" ^ completed.stderr in
  let mr_url = Text.first_url output in
  let result =
    {
      Types.name = repo.name;
      path = repo.worktree_path;
      command = completed.command;
      exit_code = completed.exit_code;
      stdout = completed.stdout;
      stderr = completed.stderr;
    }
  in
  Ok (result, mr_url)

let replace_workspace_repo (updated : Types.workspace_repo) (repos : Types.workspace_repo list) =
  List.map
    (fun (repo : Types.workspace_repo) ->
      if repo.Types.name = updated.Types.name then updated else repo)
    repos

let mr_create ~(loaded : Config.loaded) ~workspace_path ~(workspace : Types.workspace) ~repo_filters
    ?target_override () =
  let gitlab = loaded.Config.config.Types.gitlab in
  let* repos = filter_workspace_repos workspace repo_filters in
  let rec loop (results : Types.repo_command_result list) (updated_workspace : Types.workspace) =
    function
    | [] -> Ok (List.rev results, updated_workspace)
    | (repo : Types.workspace_repo) :: rest ->
        let* result, mr_url = mr_create_for_repo ~gitlab ?target_override repo in
        let updated_repo =
          match mr_url with Some url -> { repo with Types.mr_url = Some url } | None -> repo
        in
        let updated_workspace =
          {
            updated_workspace with
            Types.repos = replace_workspace_repo updated_repo updated_workspace.Types.repos;
          }
        in
        loop (result :: results) updated_workspace rest
  in
  let* results, updated_workspace = loop [] workspace repos in
  let* () = Config.save_workspace workspace_path updated_workspace in
  Ok (results, updated_workspace)

let clean ~(workspace : Types.workspace) ?(force = false) () =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (repo : Types.workspace_repo) :: rest ->
        let* completed =
          Git.worktree_remove ~force ~repo_path:repo.Types.source_path ~dest:repo.worktree_path ()
        in
        let result =
          {
            Types.name = repo.name;
            path = repo.worktree_path;
            command = completed.Proc.command;
            exit_code = completed.exit_code;
            stdout = completed.stdout;
            stderr = completed.stderr;
          }
        in
        loop (result :: acc) rest
  in
  loop [] workspace.Types.repos
