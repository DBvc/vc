open Cmdliner

let run_md5 filename =
  match Vc_hash.Hash.md5_file filename with
  | Ok digest ->
      print_endline digest;
      0
  | Error message ->
      prerr_endline ("vc hash md5: " ^ message);
      1

let filename_arg =
  let doc = "Input file to hash." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let md5_cmd =
  Cmd.v (Cmd.info "md5" ~doc:"Print an MD5 digest for a file") Term.(const run_md5 $ filename_arg)

let cmd = Cmd.group (Cmd.info "hash" ~doc:"Hash and digest utilities") [ md5_cmd ]
