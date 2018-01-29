use Mix.Config

config :aircloak_ci,
  poll_github: false,
  write_to_github: false,
  simulated_jobs: [:compile, :compliance, :test],
  cmd_runner: [console_out: true]
