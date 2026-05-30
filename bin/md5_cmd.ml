open Cmdliner

let run filename =
  try
    filename |> Digest.file |> Digest.to_hex |> print_endline;
    0
  with Sys_error message ->
    prerr_endline ("vc md5: " ^ message);
    1

let cmd =
  let filename =
    let doc = "Input file to hash." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
  in
  Cmd.v (Cmd.info "md5" ~doc:"Generate an MD5 hash for a file") Term.(const run $ filename)
