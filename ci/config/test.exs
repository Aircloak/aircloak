use Mix.Config

config :aircloak_ci,
  exec_mod: AircloakCI.TestExec,
  poll_github: false,
  write_to_github: false,
  cmd_runner: [console_out: false]
