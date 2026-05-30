let json_response ?data ?error ok =
  let fields = [ ("ok", `Bool ok) ] in
  let fields =
    match error with Some message -> fields @ [ ("error", `String message) ] | None -> fields
  in
  let fields = match data with Some data -> fields @ [ ("data", data) ] | None -> fields in
  `Assoc fields

let print_json json = print_endline (Yojson.Safe.pretty_to_string json)

let print_error ?(json = false) message =
  if json then print_json (json_response ~error:message false) else prerr_endline ("mww: " ^ message)

let print_ok_json data = print_json (json_response ~data true)

let workspace_summary_to_yojson (path, (workspace : Types.workspace)) =
  `Assoc
    [
      ("path", `String path);
      ("id", `String workspace.Types.id);
      ("title", Json_util.string_opt_to_yojson workspace.title);
      ("created_at", `String workspace.created_at);
      ("repo_count", `Int (List.length workspace.repos));
    ]

let repo_to_human (repo : Types.repo) =
  Printf.sprintf "%-18s %-40s %s" repo.Types.name
    (match repo.source_path with Some path -> path | None -> "<default>")
    repo.url

let print_repo_list (repos : Types.repo list) =
  print_endline (Printf.sprintf "%-18s %-40s %s" "repo" "source" "url");
  print_endline (String.make 90 '-');
  List.iter (fun repo -> print_endline (repo_to_human repo)) repos

let option_int = function Some i -> string_of_int i | None -> "-"
let option_string = function Some s -> s | None -> "-"

let print_workspace_list (workspaces : (string * Types.workspace) list) =
  print_endline (Printf.sprintf "%-28s %-12s %-20s %s" "workspace" "repos" "created" "title");
  print_endline (String.make 90 '-');
  List.iter
    (fun (_, workspace) ->
      print_endline
        (Printf.sprintf "%-28s %-12d %-20s %s" workspace.Types.id (List.length workspace.repos)
           workspace.created_at
           (match workspace.title with Some title -> title | None -> "")))
    workspaces

let print_status (workspace : Types.workspace) (statuses : Types.repo_status list) =
  print_endline ("Workspace: " ^ workspace.Types.id);
  (match workspace.title with Some title -> print_endline ("Title: " ^ title) | None -> ());
  print_endline "";
  print_endline
    (Printf.sprintf "%-18s %-32s %-7s %-7s %-7s %-8s %s" "repo" "branch" "dirty" "ahead" "behind"
       "exists" "mr");
  print_endline (String.make 110 '-');
  List.iter
    (fun (status : Types.repo_status) ->
      print_endline
        (Printf.sprintf "%-18s %-32s %-7s %-7s %-7s %-8s %s" status.Types.name
           (option_string status.branch)
           (if status.dirty then string_of_int status.dirty_count else "no")
           (option_int status.ahead) (option_int status.behind)
           (if status.exists then "yes" else "no")
           (option_string status.mr_url)))
    statuses

let command_results_to_yojson (results : Types.repo_command_result list) =
  `List (List.map Types.repo_command_result_to_yojson results)

let print_command_results (results : Types.repo_command_result list) =
  List.iter
    (fun result ->
      Printf.printf "\n[%s] exit=%d\n" result.Types.name result.exit_code;
      if result.stdout <> "" then Printf.printf "%s" result.stdout;
      if result.stderr <> "" then Printf.eprintf "%s" result.stderr)
    results

let any_failed (results : Types.repo_command_result list) =
  List.exists (fun r -> r.Types.exit_code <> 0) results
