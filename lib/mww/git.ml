open Resultx

let run ?cwd args = Proc.run_args ?cwd ("git" :: args)

let require ?cwd args =
  let* completed = run ?cwd args in
  Proc.require_success completed

let has_git_dir path = Fs.path_exists (Filename.concat path ".git")

let clone ~url ~dest =
  if Fs.path_exists dest then
    if has_git_dir dest then Ok None else Error (dest ^ " exists but is not a Git repository")
  else
    let* () = Fs.mkdir_p (Filename.dirname dest) in
    let* completed = run [ "clone"; url; dest ] in
    if completed.Proc.exit_code = 0 then Ok (Some completed)
    else Error (Printf.sprintf "git clone failed for %s: %s" url completed.stderr)

let fetch ~repo_path = require ~cwd:repo_path [ "fetch"; "origin" ] |> Result.map ignore

let worktree_add ~repo_path ~branch ~dest ~base =
  if Fs.path_exists dest then Error (dest ^ " already exists")
  else
    let* () = Fs.mkdir_p (Filename.dirname dest) in
    let* completed = run ~cwd:repo_path [ "worktree"; "add"; "-b"; branch; dest; base ] in
    if completed.Proc.exit_code = 0 then Ok completed
    else Error (Printf.sprintf "git worktree add failed for %s: %s" branch completed.stderr)

let worktree_remove ?(force = false) ~repo_path ~dest () =
  let args =
    if force then [ "worktree"; "remove"; "--force"; dest ] else [ "worktree"; "remove"; dest ]
  in
  let* completed = run ~cwd:repo_path args in
  if completed.Proc.exit_code = 0 then Ok completed
  else Error (Printf.sprintf "git worktree remove failed for %s: %s" dest completed.stderr)

let current_branch ~cwd =
  let* completed = run ~cwd [ "rev-parse"; "--abbrev-ref"; "HEAD" ] in
  if completed.Proc.exit_code = 0 then Ok (Some (Proc.output_trimmed completed)) else Ok None

let upstream ~cwd =
  let* completed =
    run ~cwd [ "rev-parse"; "--abbrev-ref"; "--symbolic-full-name"; "@{upstream}" ]
  in
  if completed.Proc.exit_code = 0 then Ok (Some (Proc.output_trimmed completed)) else Ok None

let status_porcelain ~cwd =
  let* completed = run ~cwd [ "status"; "--porcelain" ] in
  if completed.Proc.exit_code = 0 then Ok completed.Proc.stdout
  else Error ("git status failed: " ^ completed.stderr)

let dirty_count status_output =
  status_output |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.length

let ahead_behind ~cwd ~upstream =
  let range = upstream ^ "...HEAD" in
  let* completed = run ~cwd [ "rev-list"; "--left-right"; "--count"; range ] in
  if completed.Proc.exit_code <> 0 then Ok (None, None)
  else
    match String.split_on_char '\t' (String.trim completed.stdout) with
    | [ behind; ahead ] -> (
        match (int_of_string_opt behind, int_of_string_opt ahead) with
        | Some behind, Some ahead -> Ok (Some ahead, Some behind)
        | _ -> Ok (None, None))
    | _ -> (
        match String.split_on_char ' ' (String.trim completed.stdout) with
        | [ behind; ahead ] -> (
            match (int_of_string_opt behind, int_of_string_opt ahead) with
            | Some behind, Some ahead -> Ok (Some ahead, Some behind)
            | _ -> Ok (None, None))
        | _ -> Ok (None, None))

let push_current_branch ~cwd = run ~cwd [ "push"; "-u"; "origin"; "HEAD" ]
let version () = run [ "--version" ]
