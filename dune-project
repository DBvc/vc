(lang dune 3.20)

(name vc-cli)
(version 0.1.0)

(generate_opam_files true)

(package
 (name vc-cli)
 (synopsis "Developer command line tools")
 (description
  "vc collects small developer utilities and a multi-repo Git worktree workspace manager.")
 (maintainers "dbvcbetter@gmail.com")
 (authors "dbvcbetter@gmail.com")
 (license MIT)
 (source (github DBvc/vc))
 (depends
  (ocaml (>= 5.2))
  (cmdliner (>= 2.1.1))
  (yojson (>= 3.0.0)))
 (tags (cli git worktree gitlab workspace)))
