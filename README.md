# vc

Developer command line tools.

`vc` is an OCaml/Dune CLI. It currently provides:

- `vc md5 FILE`: print an MD5 digest for a file.
- `vc mww ...`: manage local multi-repo Git worktree workspaces.

## Build

```sh
opam install . --deps-only
dune build
dune exec ./bin/main.exe -- --help
```

## Install

The opam package is `vc-cli`; the installed executable is `vc`.

```sh
opam install .
vc --help
```

## mww

`mww` turns a multi-repository feature into a local workspace: one task directory,
many Git worktrees, independent branches and merge requests.

```sh
vc mww init ~/dev/company
cd ~/dev/company
vc mww repo add frontend git@gitlab.example.com:team/frontend.git
vc mww repo add backend git@gitlab.example.com:team/backend.git
vc mww ws new FEAT-123-login frontend backend --title "Login flow refactor"
vc mww ws status FEAT-123-login
vc mww run FEAT-123-login -- git status --short
vc mww push FEAT-123-login
vc mww mr create FEAT-123-login
```

Most `mww` commands accept `--json` for machine-readable output. Git is required;
`glab` is optional and is only needed for `vc mww mr create`.
