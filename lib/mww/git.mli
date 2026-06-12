val has_git_dir : string -> bool

val clone : url:string -> dest:string -> Proc.completed option Resultx.t

val fetch : repo_path:string -> unit Resultx.t

val ref_exists : cwd:string -> string -> bool Resultx.t

val remote_default_ref : repo_path:string -> string option Resultx.t

val worktree_add :
  repo_path:string -> branch:string -> dest:string -> base:string -> Proc.completed Resultx.t

val worktree_remove :
  ?force:bool -> repo_path:string -> dest:string -> unit -> Proc.completed Resultx.t

val current_branch : cwd:string -> string option Resultx.t

val upstream : cwd:string -> string option Resultx.t

val status_porcelain : cwd:string -> string Resultx.t

val dirty_count : string -> int

val ahead_behind : cwd:string -> upstream:string -> (int option * int option) Resultx.t

val push_current_branch : cwd:string -> Proc.completed Resultx.t

val version : unit -> Proc.completed Resultx.t
