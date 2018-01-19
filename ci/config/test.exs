use Mix.Config

config :aircloak_ci,
  docker_cleanup?: false,
  poll_github: false,
  write_to_github: false,
  simulate_commands: true,
  simulated_jobs: [:compile, :compliance, :test],
  cmd_runner: [console_out: false]
