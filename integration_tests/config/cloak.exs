use Mix.Config

config :cloak, :deploy_config_file, "test.json"

config :cloak, :api, port: 8098, address: '0.0.0.0'

config :cloak, :in_development, false

config :cloak, :alarm_handler, install: false

config :cloak, :air,
  air_site: "wss://127.0.0.1:8444",
  min_reconnect_interval: 50,
  max_reconnect_interval: 50,
  serializer: Air.CloakSocketSerializer

config :cloak, :anonymizer,
  low_count_soft_lower_bound: {5, 0},
  low_count_absolute_lower_bound: 2,
  outliers_count: {4, 0},
  min_outliers_count: 1,
  top_count: {5, 0},
  sum_noise_sigma: 0

config :cloak, :sanitize_otp_errors, false

config :cloak, :memory_limits,
  # The number of ms between consecutive memory checks.
  # This is the lower bound at which memory checks are performed.
  check_interval: 10000,
  # Once we reach this threshold, we actively start projecting
  # the memory usage and take actions if we are afraid of
  # running out of memory. The threshold is in bytes.
  limit_to_start_checks: 0,
  # The limit we are projecting towards. If we expect to reach
  # this limit within the given time, then we kill a query.
  # The threshold is in bytes.
  limit_to_check_for: 0,
  # The amount of time (in ms) we are projecting into the future.
  # If we reach the minimum memory threshold within this window
  # of time, then we start killing queries.
  allowed_minimum_time_to_limit: 3_000,
  # Time in seconds to wait between consecutive memory related query abortions
  time_between_abortions: 10_000
