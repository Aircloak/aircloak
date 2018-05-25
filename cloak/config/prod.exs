use Mix.Config

config :cloak, :deploy_config_file, "config.json"

config :cloak, :api, address: '127.0.0.1'

config :cloak, :air,
  # The reconnect intervals start out very low, and doubles for each failed attempt until
  # they arrive at and stay at this max interval.
  min_reconnect_interval: :timer.seconds(10),
  max_reconnect_interval: :timer.minutes(1)

config :cloak, :sanitize_otp_errors, true

config :cloak, :memory_limits,
  # The number of ms between consecutive memory checks.
  # This is the lower bound at which memory checks are performed.
  check_interval: 250,
  # Once we reach this threshold, we actively start projecting
  # the memory usage and take actions if we are afraid of
  # running out of memory. The threshold is in kB.
  limit_to_start_checks: 4_000_000,
  # The limit we are projecting towards. If we expect to reach
  # this limit within the given time, then we kill a query.
  # The threshold is in kB.
  limit_to_check_for: 500_000,
  # The amount of time (in ms) we are projecting into the future.
  # If we reach the minimum memory threshold within this window
  # of time, then we start killing queries.
  allowed_minimum_time_to_limit: 5_000,
  # Time in ms to wait between consecutive memory related query abortions
  time_between_abortions: 250
