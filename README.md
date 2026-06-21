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
vc ai
vc ai pick
vc ai pick --picker builtin
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
      "id": "codex-your-model",
      "title": "Your Codex Model",
      "aliases": ["codex"],
      "tool": "codex",
      "codex_profile": "your-codex-profile",
      "env": {},
      "unset_env": []
    },
    {
      "id": "claude-your-model",
      "title": "Your Claude Model",
      "aliases": ["claude"],
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

For Codex custom providers, keep the `vc` launcher profile and Codex provider
configuration separate. The `vc` JSON profile should name the Codex profile and
inject only launch-time environment variables:

```json
{
  "id": "codex-ark-glm-5-2",
  "title": "Ark GLM 5.2",
  "aliases": ["codex-glm"],
  "tool": "codex",
  "codex_profile": "ark-glm-5-2",
  "env": {
    "ARK_API_KEY": "${ARK_API_KEY}"
  },
  "unset_env": []
}
```

The Codex profile itself belongs under `~/.codex/<profile>.config.toml`:

```toml
# ~/.codex/ark-glm-5-2.config.toml
model = "glm-5.2"
model_provider = "volcengine-ark"
model_catalog_json = "/Users/me/.codex/model-catalogs/ark.json"
```

The provider definition can live in `~/.codex/config.toml`:

```toml
[model_providers.volcengine-ark]
name = "Volcengine Ark"
base_url = "https://ark.cn-beijing.volces.com/api/coding/v3"
env_key = "ARK_API_KEY"
wire_api = "responses"
```

Avoid putting `CODEX_HOME` in a `vc` Codex profile unless you intentionally want
that launched Codex process to use a different config and state root. For custom
providers that use `env_key`, avoid adding `forced_login_method = "api"` unless
you specifically want to force Codex API-key auth; it can conflict with the
normal ChatGPT web login flow. If Codex starts but warns that model metadata is
missing, add `model_catalog_json` to the Codex profile.

`vc ai` is the interactive picker entry point. It shows configured profiles as
`<title> (<Tool>)`, for example `glm-5.2 (Codex)` or `kimi-3.7 (Claude)`, then
launches the selected profile with no prompt. The text before the parentheses is
the profile title from your JSON config; you can use model-like titles for a
model-first picker display without changing profile ids or aliases.

`vc ai pick` runs the same picker explicitly. Picker mode is controlled by
`--picker auto|fzf|builtin`:

- `auto` is the default. It uses `fzf` when an `fzf` executable is present in
  `PATH`; otherwise it falls back to the builtin numbered picker.
- `fzf` requires `fzf` to be installed. If `fzf` starts and is cancelled or
  fails, `vc` reports that result instead of silently falling back.
- `builtin` always uses the numbered prompt and reads one selection from stdin.

Bare `vc ai` is interactive-only: in non-TTY contexts it exits instead of
blocking on a hidden prompt. Use `vc ai pick --picker builtin` when a script
needs to feed a selection over stdin. Picker commands do not accept a prompt for
the selected model; use `vc ai run`, `vc ai codex`, or `vc ai claude` when you
want to pass prompt text.

`vc ai doctor` reports only local facts: selected config path, whether the file
exists, profile count, env references, redacted env output, unset env entries,
and whether `codex` / `claude` are present in `PATH`. For Codex profiles it also
does read-only checks of the referenced `~/.codex/<profile>.config.toml`, legacy
profile tables in `~/.codex/config.toml`, `CODEX_HOME`, custom
`model_provider`, and `model_catalog_json`. It does not verify API credentials,
provider reachability, or whether a model actually exists.

Out of scope for `vc ai`: built-in model catalogs, implicit Codex config writes
outside `init-config`, non-interactive `exec` wrappers, `.ai-runs` artifacts,
free-form argument passthrough, and real provider/API validation.

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
