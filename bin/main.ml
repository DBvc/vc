open Cmdliner

let cmd =
  let doc = "Command line tools for local developer workflows." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "vc collects small developer utilities under one command. Use the mww subcommand for \
         multi-repo Git worktree workspaces.";
      `S Manpage.s_examples;
      `P "vc md5 README.md";
      `P "vc mww init ~/dev/company";
      `P "vc mww ws status FEAT-123-login --json";
    ]
  in
  Cmd.group (Cmd.info "vc" ~version:"0.1.0" ~doc ~man) [ Md5_cmd.cmd; Mww_cmd.cmd ]

let () = exit (Cmd.eval' cmd)
