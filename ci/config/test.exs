use Mix.Config

config :logger, level: :error

config :aircloak_ci,
  home_folder: ".test_data",
  exec_mod: AircloakCI.TestExec,
  github_api: AircloakCI.TestGithubAPI,
  poll_github: false,
  write_to_github: true,
  cmd_runner: [console_out: false],
  # Note: we're using a reduced set of components, because a single build for all components takes 0.5 seconds.
  # Since all the commands are mocked, it should be analyzed where the time is spent, and optimize the solution.
  # However, since this is not an issue in production at the moment, a simple solution has been chosen.
  components_filter: {:only, ["air", "cloak"]}
