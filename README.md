# vc

**Vita Curiosa**: a personal developer workflow console.

`vc` keeps local development workflows behind one short command while grouping capabilities by
domain.

## Command groups

- `vc mww ...`: manage local multi-repo Git worktree workspaces.
- `vc hash md5 FILE`: print an MD5 digest for a file.
- `vc ai ...`: launch Codex or Claude Code from explicit local profiles.

## Build

```sh
opam install . --deps-only
dune build
dune exec ./bin/main.exe -- --help
dune exec ./bin/main.exe -- hash md5 README.md
```

## Install

The opam package is `vc-cli`; the installed executable is `vc`.

```sh
opam install .
vc --help
```

## ai

`ai` is a small launcher for coding CLIs. It supports only two tools, `codex`
and `claude`, and it does not include built-in model profiles. Every profile
comes from a local JSON config file.

`vc ai` selects exactly one config path by this precedence:

1. `$VC_AI_CONFIG`
2. `$XDG_CONFIG_HOME/vc/ai_profiles.json`
3. `~/.config/vc/ai_profiles.json`

`vc ai` writes this file only when you explicitly run `vc ai init-config`.
Without that command it never creates or edits config for you, and it never
writes Codex config under `~/.codex`. If the selected path does not exist,
`vc ai list` reports that path and shows no profiles instead of falling through
to the next path.

```sh
vc ai list
vc ai list --json
vc ai sample-config
vc ai init-config
vc ai doctor
vc ai codex --dry-run main "summarize this repository"
vc ai claude --dry-run main "summarize this repository"
```

`vc ai init-config` writes the same placeholder JSON printed by
`vc ai sample-config` to the selected config path, creating parent directories
if needed. It refuses to overwrite an existing file; use
`vc ai init-config --force` when you intentionally want to replace that file.

Config schema version `1` uses a top-level `profiles` array. Profile ids must
be unique. Aliases are optional, but `vc ai run <name>` fails if an alias
matches more than one profile; use the full profile id to disambiguate.

```json
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-main",
      "title": "Codex / Main",
      "aliases": ["main"],
      "tool": "codex",
      "codex_profile": "your-codex-profile",
      "env": {},
      "unset_env": []
    },
    {
      "id": "claude-main",
      "title": "Claude Code / Main",
      "aliases": ["main"],
      "tool": "claude",
      "model": "your-model-name",
      "env": {
        "YOUR_BASE_URL_ENV": "${YOUR_BASE_URL}",
        "YOUR_TOKEN_ENV": "${YOUR_TOKEN}"
      },
      "unset_env": ["YOUR_CONFLICTING_TOKEN_ENV"]
    }
  ]
}
```

Codex and Claude profiles deliberately have different required fields:

- Codex profiles use `codex_profile`; `vc` passes it as
  `codex --profile <codex_profile>`. Provider and model details remain owned by
  the Codex CLI config. `vc` does not parse or validate Codex TOML.
- Claude profiles use `model`; `vc` passes it as `claude --model <model>`.
  Any environment variables needed by the selected Claude-compatible setup
  belong in that `vc` profile.

Both profile types support `env` and `unset_env`. Values in `env` may reference
`$NAME` or `${NAME}` from the current process environment. Missing references
are warnings in `vc ai doctor` and failures before a real launch. Dry runs print
the resolved command without checking whether `codex` or `claude` exists and
without starting a process. Values whose keys contain `key`, `token`, or
`secret` are redacted in launch and doctor output.

`vc ai doctor` reports only local facts: selected config path, whether the file
exists, profile count, env references, redacted env output, unset env entries,
and whether `codex` / `claude` are present in `PATH`. It does not verify API
credentials, provider reachability, or whether a model actually exists.

Out of scope for `vc ai`: built-in model catalogs, implicit config writes
outside `init-config`, `fzf` profile picking, non-interactive `exec` wrappers,
`.ai-runs` artifacts, free-form argument passthrough, and real provider/API
validation.

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
