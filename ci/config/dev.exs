use Mix.Config

config :aircloak_ci,
  poll_github: false,
  simulate_github_writes: true,
  simulated_jobs: [:compile, :compliance, :standard_test],
  cmd_runner: [console_out: true]
