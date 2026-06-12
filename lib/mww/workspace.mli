val ensure_source_clone : Config.loaded -> Types.repo -> string Resultx.t

val create :
  loaded:Config.loaded ->
  id:string ->
  ?title:string ->
  ?base:string ->
  ?branch_template:string ->
  string list ->
  (string * Types.workspace) Resultx.t

val add_repos :
  loaded:Config.loaded ->
  workspace_path:string ->
  workspace:Types.workspace ->
  ?base:string ->
  ?branch_template:string ->
  string list ->
  (string * Types.workspace) Resultx.t

val status :
  workspace:Types.workspace -> repo_filters:string list -> Types.repo_status list Resultx.t

val run_in_repos :
  workspace:Types.workspace ->
  repo_filters:string list ->
  command:string ->
  Types.repo_command_result list Resultx.t

val push :
  workspace:Types.workspace -> repo_filters:string list -> Types.repo_command_result list Resultx.t

val mr_create :
  loaded:Config.loaded ->
  workspace_path:string ->
  workspace:Types.workspace ->
  repo_filters:string list ->
  ?target_override:string ->
  unit ->
  (Types.repo_command_result list * Types.workspace) Resultx.t

val clean :
  workspace:Types.workspace -> ?force:bool -> unit -> Types.repo_command_result list Resultx.t
