use Mix.Config

config :aircloak_ci,
  exec_mod: AircloakCI.TestExec,
  github_api: AircloakCI.TestGithubAPI,
  poll_github: false,
  write_to_github: true,
  cmd_runner: [console_out: false]
