type completed = {
  command : string;
  exit_code : int;
  stdout : string;
  stderr : string;
}

let fail message = failwith message

let assert_bool message condition = if not condition then fail message

let assert_equal_string label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s\nexpected:\n%s\nactual:\n%s" label expected actual)

let assert_equal_int label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s\nexpected: %d\nactual: %d" label expected actual)

let string_contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop index =
      index + needle_len <= haystack_len
      && (String.sub haystack index needle_len = needle || loop (index + 1))
    in
    loop 0

let substring_index haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then Some 0
  else
    let rec loop index =
      if index + needle_len > haystack_len then None
      else if String.sub haystack index needle_len = needle then Some index
      else loop (index + 1)
    in
    loop 0

let assert_contains label haystack needle =
  if not (string_contains haystack needle) then
    fail (Printf.sprintf "%s\nexpected to find:\n%s\nin:\n%s" label needle haystack)

let assert_not_contains label haystack needle =
  if string_contains haystack needle then
    fail (Printf.sprintf "%s\nexpected not to find:\n%s\nin:\n%s" label needle haystack)

let assert_before label haystack first second =
  match (substring_index haystack first, substring_index haystack second) with
  | Some first_index, Some second_index when first_index < second_index -> ()
  | Some _, Some _ ->
      fail
        (Printf.sprintf "%s\nexpected this text first:\n%s\nbefore:\n%s\nin:\n%s" label
           first second haystack)
  | None, _ ->
      fail (Printf.sprintf "%s\nmissing first text:\n%s\nin:\n%s" label first haystack)
  | _, None ->
      fail (Printf.sprintf "%s\nmissing second text:\n%s\nin:\n%s" label second haystack)

let assert_starts_with label value prefix =
  match substring_index value prefix with
  | Some 0 -> ()
  | _ ->
      fail
        (Printf.sprintf "%s\nexpected value to start with:\n%s\nactual:\n%s" label prefix
           value)

let shell_quote value =
  if value = "" then "''"
  else
    let escaped = String.split_on_char '\'' value |> String.concat "'\\''" in
    "'" ^ escaped ^ "'"

let command_to_string args = String.concat " " (List.map shell_quote args)

let env_to_string env =
  env
  |> List.map (fun (key, value) -> key ^ "=" ^ shell_quote value)
  |> String.concat " "

let unset_env_to_string unset_env =
  unset_env |> List.map (fun key -> "-u " ^ shell_quote key) |> String.concat " "

let command_env_prefix env unset_env =
  match (env, unset_env) with
  | [], [] -> ""
  | _, [] -> env_to_string env ^ " "
  | _, _ ->
      let parts =
        [ unset_env_to_string unset_env; env_to_string env ]
        |> List.filter (fun value -> value <> "")
      in
      "/usr/bin/env " ^ String.concat " " parts ^ " "

let status_to_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED signal -> 128 + signal

let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let write_file path contents =
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch contents)

let file_mode path =
  let stats = Unix.stat path in
  stats.Unix.st_perm land 0o777

let remove_if_exists path = try Sys.remove path with Sys_error _ -> ()

let remove_dir_if_exists path = try Unix.rmdir path with Unix.Unix_error _ -> ()

let with_temp_config_path f =
  let seed = Filename.temp_file "vc-cli-test-ai-init-" "" in
  remove_if_exists seed;
  let dir = seed ^ ".d" in
  let path = Filename.concat dir "ai_profiles.json" in
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists path;
      remove_dir_if_exists dir)
    (fun () -> f path)

let with_temp_dir f =
  let seed = Filename.temp_file "vc-cli-test-dir-" "" in
  remove_if_exists seed;
  let dir = seed ^ ".d" in
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> remove_dir_if_exists dir) (fun () -> f dir)

let with_fake_fzf script f =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "fzf" in
      Fun.protect
        ~finally:(fun () -> remove_if_exists path)
        (fun () ->
          write_file path script;
          Unix.chmod path 0o700;
          f dir))

let run ?(env = []) ?(unset_env = []) ?stdin args =
  let stdout_file = Filename.temp_file "vc-cli-test-stdout-" ".log" in
  let stderr_file = Filename.temp_file "vc-cli-test-stderr-" ".log" in
  let stdin_file =
    match stdin with
    | None -> None
    | Some contents ->
        let path = Filename.temp_file "vc-cli-test-stdin-" ".log" in
        write_file path contents;
        Some path
  in
  let env_prefix = command_env_prefix env unset_env in
  let stdin_redirect =
    match stdin_file with None -> "" | Some path -> " < " ^ shell_quote path
  in
  let command =
    env_prefix ^ command_to_string args ^ stdin_redirect ^ " > " ^ shell_quote stdout_file
    ^ " 2> " ^ shell_quote stderr_file
  in
  let cleanup () =
    Option.iter remove_if_exists stdin_file;
    remove_if_exists stdout_file;
    remove_if_exists stderr_file
  in
  Fun.protect ~finally:cleanup (fun () ->
      let exit_code = Unix.system command |> status_to_code in
      let stdout = try read_file stdout_file with Sys_error _ -> "" in
      let stderr = try read_file stderr_file with Sys_error _ -> "" in
      { command; exit_code; stdout; stderr })

let expect_success ?env ?unset_env ?stdin args =
  let completed = run ?env ?unset_env ?stdin args in
  if completed.exit_code <> 0 then
    fail
      (Printf.sprintf "command failed: %s\nstdout:\n%s\nstderr:\n%s" completed.command
         completed.stdout completed.stderr);
  completed

let expect_failure ?env ?unset_env ?stdin args =
  let completed = run ?env ?unset_env ?stdin args in
  if completed.exit_code = 0 then
    fail
      (Printf.sprintf "command unexpectedly succeeded: %s\nstdout:\n%s\nstderr:\n%s"
         completed.command completed.stdout completed.stderr);
  completed

let with_temp_file contents f =
  let path = Filename.temp_file "vc-cli-test-" ".txt" in
  Fun.protect
    ~finally:(fun () -> remove_if_exists path)
    (fun () ->
      write_file path contents;
      f path)

let is_lower_hex_digest value =
  let rec loop index =
    index = String.length value
    ||
    match value.[index] with
    | '0' .. '9' | 'a' .. 'f' -> loop (index + 1)
    | _ -> false
  in
  String.length value = 32 && loop 0

let vc ?env ?unset_env ?stdin vc_path args =
  expect_success ?env ?unset_env ?stdin (vc_path :: args)

let vc_expect_failure ?env ?unset_env ?stdin vc_path args =
  expect_failure ?env ?unset_env ?stdin (vc_path :: args)

let test_canonical_hash vc_path =
  with_temp_file "hello from vc\n" (fun path ->
      let completed = vc vc_path [ "hash"; "md5"; path ] in
      let expected = Digest.file path |> Digest.to_hex in
      assert_equal_string "canonical hash stdout" (expected ^ "\n") completed.stdout;
      assert_equal_string "canonical hash stderr" "" completed.stderr)

let test_missing_file vc_path =
  let missing_path = Filename.temp_file "vc-cli-test-missing-" ".txt" in
  remove_if_exists missing_path;
  let completed = vc_expect_failure vc_path [ "hash"; "md5"; missing_path ] in
  assert_equal_int "missing file should exit with adapter failure" 1 completed.exit_code;
  assert_equal_string "missing file stdout" "" completed.stdout;
  assert_contains "missing file stderr should name command" completed.stderr "vc hash md5:";
  assert_contains "missing file stderr should name file" completed.stderr missing_path

let test_legacy_root_md5_removed vc_path =
  with_temp_file "legacy command should not work\n" (fun path ->
      let completed = vc_expect_failure vc_path [ "md5"; path ] in
      assert_bool "legacy root md5 should not print a digest"
        (not (is_lower_hex_digest (String.trim completed.stdout)));
      assert_contains "legacy root md5 should be an unknown command" completed.stderr
        "unknown command 'md5'")

let test_root_help vc_path =
  let completed = vc vc_path [ "--help=plain" ] in
  assert_contains "root help should describe the workflow console" completed.stdout
    "Personal developer workflow console.";
  assert_contains "root help should expose ai domain" completed.stdout "ai [COMMAND]";
  assert_contains "root help should expose hash domain" completed.stdout "hash COMMAND";
  assert_contains "root help should keep mww domain" completed.stdout "mww COMMAND";
  assert_contains "root help should show canonical hash example" completed.stdout
    "vc hash md5 README.md";
  assert_not_contains "root help should not recommend legacy md5" completed.stdout "vc md5";
  assert_not_contains "root help should not list md5 as a root command" completed.stdout
    "md5 [OPTION]"

let test_hash_help vc_path =
  let completed = vc vc_path [ "hash"; "--help=plain" ] in
  assert_contains "hash help should describe the hash command group" completed.stdout
    "vc hash COMMAND";
  assert_contains "hash help should list md5 subcommand" completed.stdout "md5";
  assert_contains "hash help should describe md5 behavior" completed.stdout
    "Print an MD5 digest for a file";
  assert_not_contains "hash help should not recommend legacy md5" completed.stdout "vc md5"

let test_ai_help vc_path =
  let completed = vc vc_path [ "ai"; "--help=plain" ] in
  assert_contains "ai help should describe the command group" completed.stdout "vc ai [COMMAND]";
  assert_contains "ai help should document no built-in profiles" completed.stdout
    "built-in model";
  assert_contains "ai help should list list command" completed.stdout "list";
  assert_contains "ai help should list sample config command" completed.stdout "sample-config";
  assert_contains "ai help should list init config command" completed.stdout "init-config";
  assert_contains "ai help should list doctor command" completed.stdout "doctor";
  assert_contains "ai help should list pick command" completed.stdout "pick";
  assert_contains "ai help should list codex command" completed.stdout "codex";
  assert_contains "ai help should list claude command" completed.stdout "claude";
  assert_contains "ai help should show doctor example" completed.stdout "vc ai doctor";
  assert_contains "ai help should show pick example" completed.stdout
    "vc ai pick --picker builtin";
  assert_contains "ai help should show dry-run example" completed.stdout
    "vc ai codex --dry-run main"

let with_missing_config f =
  let path = Filename.temp_file "vc-cli-test-ai-missing-" ".json" in
  remove_if_exists path;
  f path

let ai_config =
  {|
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-main",
      "title": "Codex Main",
      "aliases": ["main"],
      "tool": "codex",
      "codex_profile": "codex-local",
      "env": {},
      "unset_env": []
    },
    {
      "id": "claude-main",
      "title": "Claude Main",
      "aliases": ["main"],
      "tool": "claude",
      "model": "claude-local",
      "env": {},
      "unset_env": []
    }
  ]
}
|}

let ai_display_config =
  {|
{
  "version": 1,
  "profiles": [
    {
      "id": "claude-kimi-k2-7-code",
      "title": "Ark Kimi K2.7 Code",
      "aliases": ["claude-kimi"],
      "tool": "claude",
      "model": "kimi-k2.7-code",
      "env": {},
      "unset_env": []
    },
    {
      "id": "codex-deepseek-v4-pro",
      "title": "Ark DeepSeek V4 Pro",
      "aliases": ["codex-deepseek"],
      "tool": "codex",
      "codex_profile": "ark-deepseek-v4-pro",
      "env": {},
      "unset_env": []
    },
    {
      "id": "codex-glm-5-2",
      "title": "Ark GLM 5.2",
      "aliases": ["codex-glm"],
      "tool": "codex",
      "codex_profile": "ark-glm-5-2",
      "env": {},
      "unset_env": []
    },
    {
      "id": "claude-glm-5-2",
      "title": "Ark GLM 5.2",
      "aliases": ["claude-glm"],
      "tool": "claude",
      "model": "glm-5.2",
      "env": {},
      "unset_env": []
    }
  ]
}
|}

let ai_doctor_config =
  {|
{
  "version": 1,
  "profiles": [
    {
      "id": "claude-env",
      "title": "Claude Env",
      "aliases": ["env"],
      "tool": "claude",
      "model": "claude-local",
      "env": {
        "PUBLIC_BASE_URL": "https://${VC_AI_DOCTOR_HOST}/v1",
        "ANTHROPIC_AUTH_TOKEN": "${VC_AI_DOCTOR_TOKEN}",
        "OPTIONAL_FLAG": "$VC_AI_DOCTOR_MISSING"
      },
      "unset_env": ["ANTHROPIC_API_KEY"]
    }
  ]
}
|}

let ai_missing_env_run_config =
  {|
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-env",
      "title": "Codex Env",
      "aliases": [],
      "tool": "codex",
      "codex_profile": "codex-local",
      "env": {
        "OPENAI_API_KEY": "${VC_AI_REQUIRED_TOKEN}"
      },
      "unset_env": []
    }
  ]
}
|}

let codex_profile_json id =
  Printf.sprintf
    {|
    {
      "id": "%s",
      "title": "Codex",
      "aliases": [],
      "tool": "codex",
      "codex_profile": "codex-local",
      "env": {},
      "unset_env": []
    }
    |}
    id

let claude_profile_json id =
  Printf.sprintf
    {|
    {
      "id": "%s",
      "title": "Claude",
      "aliases": [],
      "tool": "claude",
      "model": "claude-local",
      "env": {},
      "unset_env": []
    }
    |}
    id

let config_with_profiles profiles =
  Printf.sprintf {|{"version": 1, "profiles": [%s]}|} (String.concat "," profiles)

let expect_ai_config_failure vc_path config expected =
  with_temp_file config (fun config_path ->
      let completed = vc_expect_failure ~env:[ ("VC_AI_CONFIG", config_path) ] vc_path [ "ai"; "list" ] in
      assert_contains "invalid ai config should fail clearly" completed.stderr expected)

let test_ai_no_builtin_profiles vc_path =
  with_missing_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let human = vc ~env vc_path [ "ai"; "list" ] in
      assert_contains "ai list should explain missing config" human.stdout
        "No AI profiles configured.";
      assert_contains "ai list should show selected config path" human.stdout config_path;
      assert_not_contains "ai list should not include built-in glm profile" human.stdout "glm";
      assert_not_contains "ai list should not include built-in deepseek profile" human.stdout
        "deepseek";
      let json = vc ~env vc_path [ "ai"; "list"; "--json" ] in
      assert_equal_string "ai list --json should be empty without config" "[]\n" json.stdout)

let test_ai_sample_config vc_path =
  let completed = vc vc_path [ "ai"; "sample-config" ] in
  assert_contains "sample config should include schema version" completed.stdout
    "\"version\": 1";
  assert_contains "sample config should include codex tool" completed.stdout "\"tool\": \"codex\"";
  assert_contains "sample config should include claude tool" completed.stdout
    "\"tool\": \"claude\"";
  assert_contains "sample config should use placeholder base url" completed.stdout
    "YOUR_CLAUDE_BASE_URL";
  assert_contains "sample config should use placeholder token" completed.stdout
    "YOUR_CLAUDE_API_TOKEN";
  assert_contains "sample config should use model-first codex title" completed.stdout
    "\"title\": \"Your Codex Model\"";
  assert_contains "sample config should use model-first claude title" completed.stdout
    "\"title\": \"Your Claude Model\"";
  assert_not_contains "sample config should not bake in glm" completed.stdout "glm";
  assert_not_contains "sample config should not bake in deepseek" completed.stdout "deepseek";
  assert_not_contains "sample config should not bake in kimi" completed.stdout "kimi"

let test_ai_init_config vc_path =
  with_temp_config_path (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let created = vc ~env vc_path [ "ai"; "init-config" ] in
      assert_contains "init-config should report created path" created.stdout
        ("Created AI profile config: " ^ config_path);
      let content = read_file config_path in
      assert_contains "init-config should write schema version" content "\"version\": 1";
      assert_contains "init-config should write codex sample" content "\"tool\": \"codex\"";
      assert_contains "init-config should write claude sample" content "\"tool\": \"claude\"";
      assert_not_contains "init-config should not bake in glm" content "glm";
      assert_equal_int "init-config should create private config file" 0o600
        (file_mode config_path);
      let listed = vc ~env vc_path [ "ai"; "list"; "--json" ] in
      assert_contains "init-config output should be parseable" listed.stdout
        "\"id\": \"codex-your-model\"";
      write_file config_path "sentinel\n";
      Unix.chmod config_path 0o644;
      let blocked = vc_expect_failure ~env vc_path [ "ai"; "init-config" ] in
      assert_contains "init-config should refuse existing config" blocked.stderr
        "config already exists";
      assert_contains "init-config should suggest force" blocked.stderr
        "vc ai init-config --force";
      assert_equal_string "init-config should not overwrite existing file" "sentinel\n"
        (read_file config_path);
      let forced = vc ~env vc_path [ "ai"; "init-config"; "--force" ] in
      assert_contains "init-config --force should report written path" forced.stdout
        ("Wrote AI profile config: " ^ config_path);
      assert_contains "init-config --force should replace existing file" (read_file config_path)
        "\"profiles\"";
      assert_equal_int "init-config --force should keep config file private" 0o600
        (file_mode config_path))

let test_ai_configured_dry_run vc_path =
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ] in
      let profiles = vc ~env vc_path [ "ai"; "list"; "--json" ] in
      assert_contains "ai list --json should include codex profile" profiles.stdout
        "\"id\": \"codex-main\"";
      assert_contains "ai list --json should include claude profile" profiles.stdout
        "\"id\": \"claude-main\"";
      let codex = vc ~env vc_path [ "ai"; "codex"; "--dry-run"; "main"; "hello"; "world" ] in
      assert_contains "codex dry-run should render selected profile" codex.stderr
        "vc ai: Codex Main";
      assert_contains "codex dry-run should render command" codex.stderr
        "$ codex --profile codex-local 'hello world'";
      assert_not_contains "codex dry-run should not check PATH" codex.stderr
        "executable not found";
      let claude = vc ~env vc_path [ "ai"; "claude"; "--dry-run"; "main"; "hello" ] in
      assert_contains "claude dry-run should render selected profile" claude.stderr
        "vc ai: Claude Main";
      assert_contains "claude dry-run should render command" claude.stderr
        "$ claude --model claude-local hello";
      assert_not_contains "claude dry-run should not check PATH" claude.stderr
        "executable not found";
      let run = vc ~env vc_path [ "ai"; "run"; "--dry-run"; "codex-main"; "hello" ] in
      assert_contains "run dry-run should resolve full profile id" run.stderr
        "$ codex --profile codex-local hello")

let test_ai_list_human_grouped_output vc_path =
  with_temp_file ai_display_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let human = vc ~env vc_path [ "ai"; "list" ] in
      assert_equal_string "ai list human should not write stderr" "" human.stderr;
      assert_contains "ai list should render codex group" human.stdout "Codex\n";
      assert_contains "ai list should render claude group" human.stdout "Claude\n";
      assert_contains "ai list should show codex target header" human.stdout "CODEX PROFILE";
      assert_contains "ai list should show claude target header" human.stdout "MODEL ID";
      assert_contains "ai list should show codex deepseek row" human.stdout
        "Ark DeepSeek V4 Pro  codex-deepseek  ark-deepseek-v4-pro";
      assert_contains "ai list should show codex glm row" human.stdout
        "Ark GLM 5.2          codex-glm       ark-glm-5-2";
      assert_contains "ai list should show claude glm row" human.stdout
        "Ark GLM 5.2         claude-glm   glm-5.2";
      assert_contains "ai list should show claude kimi row" human.stdout
        "Ark Kimi K2.7 Code  claude-kimi  kimi-k2.7-code";
      assert_before "ai list should group codex before claude" human.stdout "Codex\n" "Claude\n";
      assert_before "ai list should sort codex rows by title" human.stdout
        "Ark DeepSeek V4 Pro" "Ark GLM 5.2          codex-glm";
      assert_before "ai list should sort claude rows by title" human.stdout
        "Ark GLM 5.2         claude-glm" "Ark Kimi K2.7 Code";
      assert_not_contains "ai list human should hide old codex target label" human.stdout
        "codex_profile=";
      assert_not_contains "ai list human should hide old claude target label" human.stdout
        "model=";
      assert_not_contains "ai list human should hide old aliases suffix" human.stdout
        "aliases=";
      let json = vc ~env vc_path [ "ai"; "list"; "--json" ] in
      assert_before "ai list --json should keep config order 1" json.stdout
        "\"id\": \"claude-kimi-k2-7-code\"" "\"id\": \"codex-deepseek-v4-pro\"";
      assert_before "ai list --json should keep config order 2" json.stdout
        "\"id\": \"codex-deepseek-v4-pro\"" "\"id\": \"codex-glm-5-2\"";
      assert_before "ai list --json should keep config order 3" json.stdout
        "\"id\": \"codex-glm-5-2\"" "\"id\": \"claude-glm-5-2\"");
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let human = vc ~env vc_path [ "ai"; "list" ] in
      assert_contains "ai list should expose ambiguous codex alias fallback" human.stdout
        "Codex Main  codex-main  codex-local";
      assert_contains "ai list should expose ambiguous claude alias fallback" human.stdout
        "Claude Main  claude-main  claude-local";
      let auto = vc ~env vc_path [ "ai"; "list"; "--color"; "auto" ] in
      assert_not_contains "ai list auto color should stay plain without tty" auto.stdout "\027[";
      let never = vc ~env vc_path [ "ai"; "list"; "--color"; "never" ] in
      assert_not_contains "ai list never color should stay plain" never.stdout "\027[";
      let always = vc ~env vc_path [ "ai"; "list"; "--color"; "always" ] in
      assert_contains "ai list always color should emit ansi" always.stdout "\027[";
      assert_contains "ai list should color codex group" always.stdout "\027[1;36mCodex\027[0m";
      let json = vc ~env vc_path [ "ai"; "list"; "--json"; "--color"; "always" ] in
      assert_not_contains "ai list --json should never emit ansi" json.stdout "\027[";
      assert_contains "ai list --json should keep JSON output with color option" json.stdout
        "\"id\": \"codex-main\"";
      let invalid_color =
        vc_expect_failure ~env vc_path [ "ai"; "list"; "--color"; "bad" ]
      in
      assert_contains "ai list invalid color should fail clearly" invalid_color.stderr
        "unsupported color: bad");
  with_missing_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let invalid_color =
        vc_expect_failure ~env vc_path [ "ai"; "list"; "--color"; "bad" ]
      in
      assert_contains "ai list invalid color should not depend on config" invalid_color.stderr
        "unsupported color: bad";
      assert_not_contains "ai list invalid color should not report missing config first"
        invalid_color.stdout "No AI profiles configured.")

let test_ai_pick_builtin vc_path =
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ] in
      let picked =
        vc ~env ~stdin:"2\n" vc_path [ "ai"; "pick"; "--picker"; "builtin"; "--dry-run" ]
      in
      assert_equal_string "pick builtin should not write stdout" "" picked.stdout;
      assert_contains "pick builtin should render codex option" picked.stderr
        "1. Codex Main  Codex  codex-main  codex-local";
      assert_contains "pick builtin should render claude option" picked.stderr
        "2. Claude Main  Claude  claude-main  claude-local";
      assert_contains "pick builtin should launch selected profile title" picked.stderr
        "vc ai: Claude Main";
      assert_contains "pick builtin should not pass a prompt" picked.stderr
        "$ claude --model claude-local";
      assert_not_contains "pick builtin dry-run should not check PATH" picked.stderr
        "executable not found";
      let codex_only =
        vc ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--tool"; "codex"; "--dry-run" ]
      in
      assert_contains "pick builtin should filter codex profiles" codex_only.stderr
        "1. Codex Main  Codex  codex-main  codex-local";
      assert_not_contains "pick builtin should hide claude profiles with codex filter"
        codex_only.stderr "Claude Main  Claude";
      assert_contains "pick builtin codex filter should render codex command" codex_only.stderr
        "$ codex --profile codex-local";
      let colored =
        vc ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--color"; "always"; "--dry-run" ]
      in
      assert_contains "pick builtin always color should emit ansi" colored.stderr "\027[";
      assert_contains "pick builtin always color should still launch profile" colored.stderr
        "vc ai: Codex Main";
      let plain =
        vc ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--color"; "never"; "--dry-run" ]
      in
      assert_not_contains "pick builtin never color should stay plain" plain.stderr "\027[")

let test_ai_pick_builtin_failures vc_path =
  with_missing_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let missing =
        vc_expect_failure ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--dry-run" ]
      in
      assert_equal_int "pick missing config should exit 1" 1 missing.exit_code;
      assert_contains "pick missing config should report no profiles" missing.stderr
        "No AI profiles configured.";
      assert_contains "pick missing config should report selected path" missing.stderr config_path);
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let cancelled =
        vc_expect_failure ~env ~stdin:"" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--dry-run" ]
      in
      assert_equal_int "pick EOF should exit as cancellation" 130 cancelled.exit_code;
      assert_contains "pick EOF should report cancellation" cancelled.stderr
        "selection cancelled";
      let out_of_range =
        vc_expect_failure ~env ~stdin:"3\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--dry-run" ]
      in
      assert_equal_int "pick out of range should exit 1" 1 out_of_range.exit_code;
      assert_contains "pick out of range should report error" out_of_range.stderr
        "selection out of range: 3";
      let invalid_tool =
        vc_expect_failure ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "builtin"; "--tool"; "bad"; "--dry-run" ]
      in
      assert_contains "pick invalid tool should fail clearly" invalid_tool.stderr
        "unknown tool: bad";
      let invalid_picker =
        vc_expect_failure ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--picker"; "bad"; "--dry-run" ]
      in
      assert_contains "pick unsupported picker should fail clearly" invalid_picker.stderr
        "unsupported picker: bad";
      let invalid_color =
        vc_expect_failure ~env ~stdin:"1\n" vc_path
          [ "ai"; "pick"; "--color"; "bad"; "--dry-run" ]
      in
      assert_contains "pick unsupported color should fail clearly" invalid_color.stderr
        "unsupported color: bad")

let test_ai_default_picker_requires_tty vc_path =
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ] in
      let completed = vc_expect_failure ~env vc_path [ "ai" ] in
      assert_equal_int "default ai picker should fail without a tty" 1 completed.exit_code;
      assert_equal_string "default ai picker should not write stdout" "" completed.stdout;
      assert_contains "default ai picker should explain tty requirement" completed.stderr
        "interactive picker requires a terminal";
      assert_not_contains "default ai picker should not start builtin prompt" completed.stderr
        "Select AI profile:")

let test_ai_pick_auto_falls_back_to_builtin vc_path =
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ] in
      let picked = vc ~env ~stdin:"1\n" vc_path [ "ai"; "pick"; "--dry-run" ] in
      assert_equal_string "auto fallback should not write stdout" "" picked.stdout;
      assert_contains "auto fallback should use builtin prompt" picked.stderr
        "Select AI profile:";
      assert_contains "auto fallback should launch selected codex profile" picked.stderr
        "vc ai: Codex Main";
      assert_contains "auto fallback should render codex command" picked.stderr
        "$ codex --profile codex-local")

let fake_fzf_select_second =
  {|#!/bin/sh
i=0
while IFS= read -r line; do
  i=$((i + 1))
  if [ "$i" = "2" ]; then
    printf '%s\n' "$line"
    exit 0
  fi
done
exit 1
|}

let fake_fzf_cancel =
  {|#!/bin/sh
while IFS= read -r line; do
  :
done
exit 130
|}

let fake_fzf_fail =
  {|#!/bin/sh
while IFS= read -r line; do
  :
done
exit 2
|}

let fake_fzf_capture_first =
  {|#!/bin/sh
if [ -n "$VC_AI_FZF_ARGS" ]; then
  for arg do
    printf '%s\n' "$arg" >> "$VC_AI_FZF_ARGS"
  done
fi

i=0
selection=
while IFS= read -r line; do
  if [ -n "$VC_AI_FZF_INPUT" ]; then
    printf '%s\n' "$line" >> "$VC_AI_FZF_INPUT"
  fi
  i=$((i + 1))
  if [ "$i" = "1" ]; then
    selection=$line
  fi
done

if [ "$selection" = "" ]; then
  exit 1
fi

printf '%s\n' "$selection"
|}

let test_ai_pick_fzf_and_auto vc_path =
  with_temp_file ai_config (fun config_path ->
      with_fake_fzf fake_fzf_select_second (fun path ->
          let env = [ ("VC_AI_CONFIG", config_path); ("PATH", path) ] in
          let auto = vc ~env vc_path [ "ai"; "pick"; "--dry-run" ] in
          assert_equal_string "auto fzf should not write stdout" "" auto.stdout;
          assert_contains "auto should use fake fzf selection" auto.stderr
            "vc ai: Claude Main";
          assert_contains "auto fzf should render claude command" auto.stderr
            "$ claude --model claude-local";
          assert_not_contains "auto fzf should not fall back to builtin when fzf exists"
            auto.stderr "Select AI profile:";
          let explicit = vc ~env vc_path [ "ai"; "pick"; "--picker"; "fzf"; "--dry-run" ] in
          assert_contains "explicit fzf should use fake fzf selection" explicit.stderr
            "vc ai: Claude Main";
          assert_not_contains "explicit fzf should not use builtin prompt" explicit.stderr
            "Select AI profile:");
      with_fake_fzf fake_fzf_capture_first (fun path ->
          with_temp_file "" (fun args_path ->
              with_temp_file "" (fun input_path ->
                  let env =
                    [
                      ("VC_AI_CONFIG", config_path);
                      ("PATH", path);
                      ("VC_AI_FZF_ARGS", args_path);
                      ("VC_AI_FZF_INPUT", input_path);
                    ]
                  in
                  let colored =
                    vc ~env vc_path
                      [ "ai"; "pick"; "--picker"; "fzf"; "--color"; "always"; "--dry-run" ]
                  in
                  assert_contains "colored fzf should launch selected profile" colored.stderr
                    "vc ai: Codex Main";
                  let args = read_file args_path in
                  let input = read_file input_path in
                  assert_contains "colored fzf should enable ansi mode" args "--ansi\n";
                  assert_contains "colored fzf input should contain ansi" input "\027[";
                  let first_line =
                    match String.split_on_char '\n' input with line :: _ -> line | [] -> ""
                  in
                  assert_starts_with "fzf hidden index should stay plain" first_line "1\t")));
      with_fake_fzf fake_fzf_capture_first (fun path ->
          with_temp_file "" (fun args_path ->
              with_temp_file "" (fun input_path ->
                  let env =
                    [
                      ("VC_AI_CONFIG", config_path);
                      ("PATH", path);
                      ("VC_AI_FZF_ARGS", args_path);
                      ("VC_AI_FZF_INPUT", input_path);
                    ]
                  in
                  let plain =
                    vc ~env vc_path
                      [ "ai"; "pick"; "--picker"; "fzf"; "--color"; "never"; "--dry-run" ]
                  in
                  assert_contains "plain fzf should launch selected profile" plain.stderr
                    "vc ai: Codex Main";
                  let args = read_file args_path in
                  let input = read_file input_path in
                  assert_not_contains "plain fzf should not enable ansi mode" args "--ansi";
                  assert_not_contains "plain fzf input should not contain ansi" input "\027["))))

let test_ai_pick_fzf_failures vc_path =
  with_temp_file ai_config (fun config_path ->
      let missing_env =
        [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ]
      in
      let missing =
        vc_expect_failure ~env:missing_env vc_path
          [ "ai"; "pick"; "--picker"; "fzf"; "--dry-run" ]
      in
      assert_equal_int "explicit fzf should use command-not-found exit" 127 missing.exit_code;
      assert_contains "explicit fzf should report missing fzf" missing.stderr
        "fzf picker requested but fzf was not found in PATH";
      assert_not_contains "explicit fzf should not fall back to builtin" missing.stderr
        "Select AI profile:";
      with_fake_fzf fake_fzf_cancel (fun path ->
          let env = [ ("VC_AI_CONFIG", config_path); ("PATH", path) ] in
          let cancelled =
            vc_expect_failure ~env vc_path [ "ai"; "pick"; "--picker"; "fzf"; "--dry-run" ]
          in
          assert_equal_int "fzf cancellation should exit 130" 130 cancelled.exit_code;
          assert_contains "fzf cancellation should be reported" cancelled.stderr
            "selection cancelled");
      with_fake_fzf fake_fzf_fail (fun path ->
          let env = [ ("VC_AI_CONFIG", config_path); ("PATH", path) ] in
          let failed =
            vc_expect_failure ~env vc_path [ "ai"; "pick"; "--picker"; "fzf"; "--dry-run" ]
          in
          assert_equal_int "fzf failure should exit 1" 1 failed.exit_code;
          assert_contains "fzf failure should be reported" failed.stderr
            "fzf failed with exit code 2"))

let test_ai_doctor vc_path =
  with_temp_file ai_doctor_config (fun config_path ->
      let env =
        [
          ("VC_AI_CONFIG", config_path);
          ("PATH", "/definitely-missing-vc-ai-test");
          ("VC_AI_DOCTOR_HOST", "localhost.test");
          ("VC_AI_DOCTOR_TOKEN", "super-secret-token");
        ]
      in
      let completed =
        vc ~env ~unset_env:[ "VC_AI_DOCTOR_MISSING" ] vc_path [ "ai"; "doctor" ]
      in
      assert_contains "doctor should report config path" completed.stdout
        ("Config: " ^ config_path);
      assert_contains "doctor should report config found" completed.stdout
        "Config file: found";
      assert_contains "doctor should report profile count" completed.stdout "Profiles: 1";
      assert_contains "doctor should report missing codex binary" completed.stdout
        "binary codex: missing in PATH (warning)";
      assert_contains "doctor should report missing claude binary" completed.stdout
        "binary claude: missing in PATH (warning)";
      assert_contains "doctor should report profile" completed.stdout
        "profile claude-env (claude): Claude Env";
      assert_contains "doctor should expand non-secret env" completed.stdout
        "env PUBLIC_BASE_URL=https://localhost.test/v1";
      assert_contains "doctor should redact secret env" completed.stdout
        "env ANTHROPIC_AUTH_TOKEN=<redacted>";
      assert_not_contains "doctor should not print secret env value" completed.stdout
        "super-secret-token";
      assert_contains "doctor should report set env refs" completed.stdout
        "env ref VC_AI_DOCTOR_TOKEN: set";
      assert_contains "doctor should warn for missing env refs" completed.stdout
        "env ref VC_AI_DOCTOR_MISSING: missing (warning)";
      assert_contains "doctor should report unset env" completed.stdout
        "unset ANTHROPIC_API_KEY")

let ai_doctor_codex_config codex_home =
  Printf.sprintf
    {|
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-ark",
      "title": "Ark GLM 5.2",
      "aliases": ["codex-glm"],
      "tool": "codex",
      "codex_profile": "ark-glm-5-2",
      "env": {
        "CODEX_HOME": "%s",
        "ARK_API_KEY": "${ARK_API_KEY}"
      },
      "unset_env": []
    }
  ]
}
|}
    codex_home

let test_ai_doctor_codex_profile_config vc_path =
  with_temp_dir (fun dir ->
      let codex_home = Filename.concat dir ".codex" in
      Unix.mkdir codex_home 0o700;
      let main_config = Filename.concat codex_home "config.toml" in
      let profile_config = Filename.concat codex_home "ark-glm-5-2.config.toml" in
      write_file main_config
        {|
profile = "legacy"

[model_providers.volcengine-ark]
name = "Volcengine Ark"
base_url = "https://ark.example.test/v1"
env_key = "ARK_API_KEY"
wire_api = "responses"

[profiles.legacy]
model = "old"
|};
      write_file profile_config
        {|
model = "glm-5.2"
model_provider = "volcengine-ark"
forced_login_method = "api"
|};
      Fun.protect
        ~finally:(fun () ->
          remove_if_exists profile_config;
          remove_if_exists main_config;
          remove_dir_if_exists codex_home)
        (fun () ->
          with_temp_file (ai_doctor_codex_config codex_home) (fun config_path ->
              let env =
                [
                  ("VC_AI_CONFIG", config_path);
                  ("PATH", "/definitely-missing-vc-ai-test");
                  ("ARK_API_KEY", "ark-secret");
                ]
              in
              let completed = vc ~env vc_path [ "ai"; "doctor" ] in
              assert_contains "doctor should report codex profile" completed.stdout
                "profile codex-ark (codex): Ark GLM 5.2";
              assert_contains "doctor should report codex home" completed.stdout
                ("codex home: " ^ codex_home);
              assert_contains "doctor should warn about CODEX_HOME" completed.stdout
                "this vc profile sets CODEX_HOME";
              assert_contains "doctor should find codex main config" completed.stdout
                ("codex main config: found " ^ main_config);
              assert_contains "doctor should find codex profile file" completed.stdout
                ("codex profile file: found " ^ profile_config);
              assert_contains "doctor should warn about legacy profile selector" completed.stdout
                "legacy profile = ... selector";
              assert_contains "doctor should warn about legacy profiles tables" completed.stdout
                "legacy [profiles.*] tables";
              assert_contains "doctor should report model provider" completed.stdout
                "codex model_provider: volcengine-ark";
              assert_not_contains "doctor should see provider definition" completed.stdout
                "model_provider=volcengine-ark but [model_providers.volcengine-ark] was not found";
              assert_contains "doctor should warn about forced api login" completed.stdout
                "forced_login_method=api can conflict with ChatGPT web login";
              assert_contains "doctor should warn about missing model catalog" completed.stdout
                "custom model_provider has no model_catalog_json";
              assert_not_contains "doctor should redact ark token" completed.stdout "ark-secret")))

let test_ai_invalid_config_failures vc_path =
  let duplicate_id =
    config_with_profiles [ codex_profile_json "repeat"; claude_profile_json "repeat" ]
  in
  expect_ai_config_failure vc_path duplicate_id "duplicate profile id: repeat";
  let unknown_field =
    {|
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-extra",
      "title": "Codex Extra",
      "aliases": [],
      "tool": "codex",
      "codex_profile": "codex-local",
      "extra": true,
      "env": {},
      "unset_env": []
    }
  ]
}
|}
  in
  expect_ai_config_failure vc_path unknown_field "profiles[0]: unknown field(s): extra";
  let codex_with_model =
    {|
{
  "version": 1,
  "profiles": [
    {
      "id": "codex-model",
      "title": "Codex Model",
      "aliases": [],
      "tool": "codex",
      "codex_profile": "codex-local",
      "model": "not-allowed",
      "env": {},
      "unset_env": []
    }
  ]
}
|}
  in
  expect_ai_config_failure vc_path codex_with_model
    "profiles[0]: field model is not allowed for codex profiles";
  let claude_with_codex_profile =
    {|
{
  "version": 1,
  "profiles": [
    {
      "id": "claude-codex-profile",
      "title": "Claude Codex Profile",
      "aliases": [],
      "tool": "claude",
      "model": "claude-local",
      "codex_profile": "not-allowed",
      "env": {},
      "unset_env": []
    }
  ]
}
|}
  in
  expect_ai_config_failure vc_path claude_with_codex_profile
    "profiles[0]: field codex_profile is not allowed for claude profiles"

let test_ai_ambiguous_alias vc_path =
  with_temp_file ai_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path) ] in
      let completed = vc_expect_failure ~env vc_path [ "ai"; "run"; "--dry-run"; "main" ] in
      assert_contains "ambiguous alias should name alias" completed.stderr
        "ambiguous profile alias: main";
      assert_contains "ambiguous alias should name codex match" completed.stderr "codex-main";
      assert_contains "ambiguous alias should name claude match" completed.stderr "claude-main")

let test_ai_missing_env_blocks_real_run vc_path =
  with_temp_file ai_missing_env_run_config (fun config_path ->
      let env = [ ("VC_AI_CONFIG", config_path); ("PATH", "/definitely-missing-vc-ai-test") ] in
      let completed =
        vc_expect_failure ~env ~unset_env:[ "VC_AI_REQUIRED_TOKEN" ] vc_path
          [ "ai"; "codex"; "codex-env" ]
      in
      assert_equal_int "missing env should exit before launch" 1 completed.exit_code;
      assert_contains "missing env should name profile" completed.stderr
        "missing environment variable(s) for codex-env";
      assert_contains "missing env should name variable" completed.stderr
        "VC_AI_REQUIRED_TOKEN";
      assert_not_contains "missing env should fail before binary lookup" completed.stderr
        "executable not found")

let run_test name f =
  try
    f ();
    Printf.printf "ok - %s\n%!" name
  with exn ->
    Printf.eprintf "not ok - %s\n%s\n%!" name (Printexc.to_string exn);
    exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _; vc_path ] ->
      let vc_path =
        if Filename.is_relative vc_path then Filename.concat (Sys.getcwd ()) vc_path else vc_path
      in
      run_test "canonical hash command" (fun () -> test_canonical_hash vc_path);
      run_test "missing file failure" (fun () -> test_missing_file vc_path);
      run_test "legacy root md5 removal" (fun () -> test_legacy_root_md5_removed vc_path);
      run_test "root help" (fun () -> test_root_help vc_path);
      run_test "hash help" (fun () -> test_hash_help vc_path);
      run_test "ai help" (fun () -> test_ai_help vc_path);
      run_test "ai has no built-in profiles" (fun () -> test_ai_no_builtin_profiles vc_path);
      run_test "ai sample config" (fun () -> test_ai_sample_config vc_path);
      run_test "ai init config" (fun () -> test_ai_init_config vc_path);
      run_test "ai configured dry-run" (fun () -> test_ai_configured_dry_run vc_path);
      run_test "ai list human grouped output" (fun () ->
          test_ai_list_human_grouped_output vc_path);
      run_test "ai pick builtin" (fun () -> test_ai_pick_builtin vc_path);
      run_test "ai pick builtin failures" (fun () -> test_ai_pick_builtin_failures vc_path);
      run_test "ai default picker requires tty" (fun () ->
          test_ai_default_picker_requires_tty vc_path);
      run_test "ai pick auto falls back to builtin" (fun () ->
          test_ai_pick_auto_falls_back_to_builtin vc_path);
      run_test "ai pick fzf and auto" (fun () -> test_ai_pick_fzf_and_auto vc_path);
      run_test "ai pick fzf failures" (fun () -> test_ai_pick_fzf_failures vc_path);
      run_test "ai doctor" (fun () -> test_ai_doctor vc_path);
      run_test "ai doctor codex profile config" (fun () ->
          test_ai_doctor_codex_profile_config vc_path);
      run_test "ai invalid config failures" (fun () -> test_ai_invalid_config_failures vc_path);
      run_test "ai ambiguous alias" (fun () -> test_ai_ambiguous_alias vc_path);
      run_test "ai missing env blocks real run" (fun () -> test_ai_missing_env_blocks_real_run vc_path)
  | _ -> fail "usage: cli_regression_tests <path-to-vc>"
