open Resultx

let filename = "mww.json"
let workspace_filename = ".mww-workspace.json"

type loaded = { path : string; root_dir : string; config : Types.config }

let resolve_base config_path (config : Types.config) =
  let config_dir = Filename.dirname config_path in
  if Filename.is_relative config.Types.root then Filename.concat config_dir config.root
  else config.root

let resolve_config_path ?config_path () =
  match config_path with
  | Some path -> Ok path
  | None -> (
      match Fs.find_up ~filename with
      | Some path -> Ok path
      | None -> Error ("cannot find " ^ filename ^ " from current directory"))

let load ?config_path () =
  let* path = resolve_config_path ?config_path () in
  let* config = Json_util.parse_file path Types.config_of_yojson in
  Ok { path; root_dir = resolve_base path config; config }

let save path (config : Types.config) = Json_util.write_file path (Types.config_to_yojson config)

let init ?(root = ".") ?config_path () =
  let root = Fs.absolutize root in
  let path = match config_path with Some path -> path | None -> Filename.concat root filename in
  if Fs.path_exists path then Error (path ^ " already exists")
  else
    let config = Types.default_config ~root:"." () in
    let* () = Fs.mkdir_p root in
    let* () = save path config in
    let* () = Fs.mkdir_p (Filename.concat root config.repos_dir) in
    let* () = Fs.mkdir_p (Filename.concat root config.workspaces_dir) in
    Ok { path; root_dir = root; config }

let config_to_workspace_dir (loaded : loaded) =
  Filename.concat loaded.root_dir loaded.config.Types.workspaces_dir

let config_to_repos_dir (loaded : loaded) =
  Filename.concat loaded.root_dir loaded.config.Types.repos_dir

let repo_source_path (loaded : loaded) (repo : Types.repo) =
  match repo.Types.source_path with
  | Some path when Filename.is_relative path -> Filename.concat loaded.root_dir path
  | Some path -> path
  | None -> Filename.concat (config_to_repos_dir loaded) repo.name

let workspace_root (loaded : loaded) id = Filename.concat (config_to_workspace_dir loaded) id
let workspace_meta_path workspace_root = Filename.concat workspace_root workspace_filename
let workspace_path (loaded : loaded) id = workspace_meta_path (workspace_root loaded id)
let find_workspace_meta_up () = Fs.find_up ~filename:workspace_filename
let load_workspace_by_path path = Json_util.parse_file path Types.workspace_of_yojson

let load_workspace ?id (loaded : loaded) =
  let path =
    match id with
    | Some id -> workspace_path loaded id
    | None -> (
        match find_workspace_meta_up () with Some path -> path | None -> workspace_path loaded "")
  in
  if path = workspace_path loaded "" then
    Error "workspace id is required when not running inside a workspace directory"
  else
    let* workspace = load_workspace_by_path path in
    Ok (path, workspace)

let save_workspace path (workspace : Types.workspace) =
  Json_util.write_file path (Types.workspace_to_yojson workspace)

let find_repo (loaded : loaded) name =
  loaded.config.repos |> List.find_opt (fun (repo : Types.repo) -> repo.Types.name = name)

let upsert_repo (loaded : loaded) (repo : Types.repo) =
  let repos =
    let without =
      List.filter (fun (r : Types.repo) -> r.Types.name <> repo.Types.name) loaded.config.repos
    in
    List.sort
      (fun (a : Types.repo) (b : Types.repo) -> String.compare a.Types.name b.Types.name)
      (repo :: without)
  in
  let config = { loaded.config with Types.repos } in
  save loaded.path config |> Result.map (fun () -> { loaded with config })

let list_workspaces (loaded : loaded) =
  let dir = config_to_workspace_dir loaded in
  let* dirs = Fs.list_dirs dir in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | id :: rest ->
        let meta = workspace_path loaded id in
        if Fs.path_exists meta then
          let* workspace = load_workspace_by_path meta in
          loop ((meta, workspace) :: acc) rest
        else loop acc rest
  in
  loop [] dirs
