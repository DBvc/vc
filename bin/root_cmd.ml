open Cmdliner

let cmd =
  let doc = "Personal developer workflow console." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "vc keeps local development workflows behind one short command while grouping capabilities \
         by domain.";
      `S Manpage.s_examples;
      `P "vc mww init ~/dev/company";
      `P "vc mww ws status FEAT-123-login --json";
      `P "vc hash md5 README.md";
    ]
  in
  Cmd.group (Cmd.info "vc" ~version:"0.1.0" ~doc ~man) [ Mww_cmd.cmd; Hash_cmd.cmd ]
