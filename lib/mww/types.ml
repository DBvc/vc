open Resultx
module J = Json_util

type repo = {
  name : string;
  url : string;
  source_path : string option;
  default_base : string option;
  target_branch : string option;
  enabled : bool;
}

type gitlab_config = {
  mr_tool : string;
  draft : bool;
  yes : bool;
  remove_source_branch : bool;
  labels : string list;
}

type config = {
  version : int;
  root : string;
  repos_dir : string;
  workspaces_dir : string;
  default_base : string;
  default_target_branch : string;
  branch_template : string;
  gitlab : gitlab_config;
  repos : repo list;
}

type workspace_repo = {
  name : string;
  url : string;
  source_path : string;
  worktree_path : string;
  base : string;
  branch : string;
  target_branch : string;
  mr_url : string option;
}

type workspace = {
  version : int;
  id : string;
  title : string option;
  created_at : string;
  repos : workspace_repo list;
}

type repo_status = {
  name : string;
  path : string;
  branch : string option;
  upstream : string option;
  dirty : bool;
  dirty_count : int;
  ahead : int option;
  behind : int option;
  mr_url : string option;
  exists : bool;
}

type repo_command_result = {
  name : string;
  path : string;
  command : string;
  exit_code : int;
  stdout : string;
  stderr : string;
}

let default_gitlab_config =
  { mr_tool = "glab"; draft = true; yes = true; remove_source_branch = false; labels = [] }

let default_config ?(root = ".") () =
  {
    version = 1;
    root;
    repos_dir = "repos";
    workspaces_dir = "workspaces";
    default_base = "origin/main";
    default_target_branch = "main";
    branch_template = "{user}/{workspace}/{repo}";
    gitlab = default_gitlab_config;
    repos = [];
  }

let repo_to_yojson (repo : repo) =
  `Assoc
    [
      ("name", `String repo.name);
      ("url", `String repo.url);
      ("source_path", J.string_opt_to_yojson repo.source_path);
      ("default_base", J.string_opt_to_yojson repo.default_base);
      ("target_branch", J.string_opt_to_yojson repo.target_branch);
      ("enabled", `Bool repo.enabled);
    ]

let repo_of_yojson json =
  let* name = J.required_string "name" json in
  let* url = J.required_string "url" json in
  let* source_path = J.optional_string "source_path" json in
  let* default_base = J.optional_string "default_base" json in
  let* target_branch = J.optional_string "target_branch" json in
  let* enabled = J.bool_with_default ~default:true "enabled" json in
  Ok { name; url; source_path; default_base; target_branch; enabled }

let gitlab_to_yojson (gitlab : gitlab_config) =
  `Assoc
    [
      ("mr_tool", `String gitlab.mr_tool);
      ("draft", `Bool gitlab.draft);
      ("yes", `Bool gitlab.yes);
      ("remove_source_branch", `Bool gitlab.remove_source_branch);
      ("labels", `List (List.map (fun label -> `String label) gitlab.labels));
    ]

let gitlab_of_yojson json =
  let* mr_tool = J.string_with_default ~default:"glab" "mr_tool" json in
  let* draft = J.bool_with_default ~default:true "draft" json in
  let* yes = J.bool_with_default ~default:true "yes" json in
  let* remove_source_branch = J.bool_with_default ~default:false "remove_source_branch" json in
  let* labels = J.string_list_with_default ~default:[] "labels" json in
  Ok { mr_tool; draft; yes; remove_source_branch; labels }

let config_to_yojson (config : config) =
  `Assoc
    [
      ("version", `Int config.version);
      ("root", `String config.root);
      ("repos_dir", `String config.repos_dir);
      ("workspaces_dir", `String config.workspaces_dir);
      ("default_base", `String config.default_base);
      ("default_target_branch", `String config.default_target_branch);
      ("branch_template", `String config.branch_template);
      ("gitlab", gitlab_to_yojson config.gitlab);
      ("repos", `List (List.map repo_to_yojson config.repos));
    ]

let config_of_yojson json =
  let* version = J.int_with_default ~default:1 "version" json in
  let* root = J.string_with_default ~default:"." "root" json in
  let* repos_dir = J.string_with_default ~default:"repos" "repos_dir" json in
  let* workspaces_dir = J.string_with_default ~default:"workspaces" "workspaces_dir" json in
  let* default_base = J.string_with_default ~default:"origin/main" "default_base" json in
  let* default_target_branch = J.string_with_default ~default:"main" "default_target_branch" json in
  let* branch_template =
    J.string_with_default ~default:"{user}/{workspace}/{repo}" "branch_template" json
  in
  let* gitlab =
    match J.member "gitlab" json with
    | None | Some `Null -> Ok default_gitlab_config
    | Some value -> gitlab_of_yojson value
  in
  let* repos = J.list_with_default ~default:[] "repos" json repo_of_yojson in
  Ok
    {
      version;
      root;
      repos_dir;
      workspaces_dir;
      default_base;
      default_target_branch;
      branch_template;
      gitlab;
      repos;
    }

let workspace_repo_to_yojson (repo : workspace_repo) =
  `Assoc
    [
      ("name", `String repo.name);
      ("url", `String repo.url);
      ("source_path", `String repo.source_path);
      ("worktree_path", `String repo.worktree_path);
      ("base", `String repo.base);
      ("branch", `String repo.branch);
      ("target_branch", `String repo.target_branch);
      ("mr_url", J.string_opt_to_yojson repo.mr_url);
    ]

let workspace_repo_of_yojson json =
  let* name = J.required_string "name" json in
  let* url = J.required_string "url" json in
  let* source_path = J.required_string "source_path" json in
  let* worktree_path = J.required_string "worktree_path" json in
  let* base = J.required_string "base" json in
  let* branch = J.required_string "branch" json in
  let* target_branch = J.required_string "target_branch" json in
  let* mr_url = J.optional_string "mr_url" json in
  Ok { name; url; source_path; worktree_path; base; branch; target_branch; mr_url }

let workspace_to_yojson (workspace : workspace) =
  `Assoc
    [
      ("version", `Int workspace.version);
      ("id", `String workspace.id);
      ("title", J.string_opt_to_yojson workspace.title);
      ("created_at", `String workspace.created_at);
      ("repos", `List (List.map workspace_repo_to_yojson workspace.repos));
    ]

let workspace_of_yojson json =
  let* version = J.int_with_default ~default:1 "version" json in
  let* id = J.required_string "id" json in
  let* title = J.optional_string "title" json in
  let* created_at = J.string_with_default ~default:"" "created_at" json in
  let* repos = J.list_with_default ~default:[] "repos" json workspace_repo_of_yojson in
  Ok { version; id; title; created_at; repos }

let repo_status_to_yojson (status : repo_status) =
  `Assoc
    [
      ("name", `String status.name);
      ("path", `String status.path);
      ("branch", J.string_opt_to_yojson status.branch);
      ("upstream", J.string_opt_to_yojson status.upstream);
      ("dirty", `Bool status.dirty);
      ("dirty_count", `Int status.dirty_count);
      ("ahead", match status.ahead with Some value -> `Int value | None -> `Null);
      ("behind", match status.behind with Some value -> `Int value | None -> `Null);
      ("mr_url", J.string_opt_to_yojson status.mr_url);
      ("exists", `Bool status.exists);
    ]

let repo_command_result_to_yojson (result : repo_command_result) =
  `Assoc
    [
      ("name", `String result.name);
      ("path", `String result.path);
      ("command", `String result.command);
      ("exit_code", `Int result.exit_code);
      ("stdout", `String result.stdout);
      ("stderr", `String result.stderr);
    ]
