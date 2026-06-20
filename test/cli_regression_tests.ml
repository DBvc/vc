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

let assert_contains label haystack needle =
  if not (string_contains haystack needle) then
    fail (Printf.sprintf "%s\nexpected to find:\n%s\nin:\n%s" label needle haystack)

let assert_not_contains label haystack needle =
  if string_contains haystack needle then
    fail (Printf.sprintf "%s\nexpected not to find:\n%s\nin:\n%s" label needle haystack)

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

let remove_if_exists path = try Sys.remove path with Sys_error _ -> ()

let run ?(env = []) ?(unset_env = []) args =
  let stdout_file = Filename.temp_file "vc-cli-test-stdout-" ".log" in
  let stderr_file = Filename.temp_file "vc-cli-test-stderr-" ".log" in
  let env_prefix = command_env_prefix env unset_env in
  let command =
    env_prefix ^ command_to_string args ^ " > " ^ shell_quote stdout_file ^ " 2> "
    ^ shell_quote stderr_file
  in
  let cleanup () =
    remove_if_exists stdout_file;
    remove_if_exists stderr_file
  in
  Fun.protect ~finally:cleanup (fun () ->
      let exit_code = Unix.system command |> status_to_code in
      let stdout = try read_file stdout_file with Sys_error _ -> "" in
      let stderr = try read_file stderr_file with Sys_error _ -> "" in
      { command; exit_code; stdout; stderr })

let expect_success ?env ?unset_env args =
  let completed = run ?env ?unset_env args in
  if completed.exit_code <> 0 then
    fail
      (Printf.sprintf "command failed: %s\nstdout:\n%s\nstderr:\n%s" completed.command
         completed.stdout completed.stderr);
  completed

let expect_failure ?env ?unset_env args =
  let completed = run ?env ?unset_env args in
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

let vc ?env ?unset_env vc_path args = expect_success ?env ?unset_env (vc_path :: args)

let vc_expect_failure ?env ?unset_env vc_path args =
  expect_failure ?env ?unset_env (vc_path :: args)

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
  let completed = vc vc_path [ "--help" ] in
  assert_contains "root help should describe the workflow console" completed.stdout
    "Personal developer workflow console.";
  assert_contains "root help should expose ai domain" completed.stdout "ai COMMAND";
  assert_contains "root help should expose hash domain" completed.stdout "hash COMMAND";
  assert_contains "root help should keep mww domain" completed.stdout "mww COMMAND";
  assert_contains "root help should show canonical hash example" completed.stdout
    "vc hash md5 README.md";
  assert_not_contains "root help should not recommend legacy md5" completed.stdout "vc md5";
  assert_not_contains "root help should not list md5 as a root command" completed.stdout
    "md5 [OPTION]"

let test_hash_help vc_path =
  let completed = vc vc_path [ "hash"; "--help" ] in
  assert_contains "hash help should describe the hash command group" completed.stdout
    "vc hash COMMAND";
  assert_contains "hash help should list md5 subcommand" completed.stdout "md5";
  assert_contains "hash help should describe md5 behavior" completed.stdout
    "Print an MD5 digest for a file";
  assert_not_contains "hash help should not recommend legacy md5" completed.stdout "vc md5"

let test_ai_help vc_path =
  let completed = vc vc_path [ "ai"; "--help" ] in
  assert_contains "ai help should describe the command group" completed.stdout "vc ai COMMAND";
  assert_contains "ai help should document no built-in profiles" completed.stdout
    "built-in model";
  assert_contains "ai help should list list command" completed.stdout "list";
  assert_contains "ai help should list sample config command" completed.stdout "sample-config";
  assert_contains "ai help should list doctor command" completed.stdout "doctor";
  assert_contains "ai help should list codex command" completed.stdout "codex";
  assert_contains "ai help should list claude command" completed.stdout "claude";
  assert_contains "ai help should show doctor example" completed.stdout "vc ai doctor";
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
  assert_not_contains "sample config should not bake in glm" completed.stdout "glm";
  assert_not_contains "sample config should not bake in deepseek" completed.stdout "deepseek";
  assert_not_contains "sample config should not bake in kimi" completed.stdout "kimi"

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
      run_test "ai configured dry-run" (fun () -> test_ai_configured_dry_run vc_path);
      run_test "ai doctor" (fun () -> test_ai_doctor vc_path);
      run_test "ai invalid config failures" (fun () -> test_ai_invalid_config_failures vc_path);
      run_test "ai ambiguous alias" (fun () -> test_ai_ambiguous_alias vc_path);
      run_test "ai missing env blocks real run" (fun () -> test_ai_missing_env_blocks_real_run vc_path)
  | _ -> fail "usage: cli_regression_tests <path-to-vc>"
