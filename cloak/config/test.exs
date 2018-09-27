use Mix.Config

config :cloak, :deploy_config_file, "test.json"

# Print only errors during test
config :logger, level: :error

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  air_site: "ws://127.0.0.1:29876",
  min_reconnect_interval: 50,
  max_reconnect_interval: 50,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.Json

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  low_count_soft_lower_bound: {5, 0},

  # The minimum absolute value of the noisy lower bound.
  low_count_absolute_lower_bound: 2,

  # The range and standard deviation for the count of outliers dropped, from the top of the collection,
  # during the anonymized aggregation of values. The outliers are replaced with the noisy
  # average value for the top of the remaining users in the collection.
  outliers_count: {4, 4, 0},

  # The range and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {5, 5, 0},

  # The standard deviation for the noisy top average added to summed values.
  sum_noise_sigma: 0,

  # The scale factor for the `sum_noise_sigma`.
  # Takes the form: {average_factor, top_average_factor}
  sum_noise_sigma_scale_params: {0, 0},

  # This values specifies the isolating factor threshold. See `Isolating columns` in anonymization.md.
  isolating_column_threshold: 1

config :cloak, :data_source,
  timeout: :timer.hours(12),
  batch_size: 100,
  connect_timeout: :timer.seconds(15),
  connection_keep_time: :timer.minutes(1)

config :excheck, :number_iterations, 200
config :cloak, :sanitize_otp_errors, false

config :cloak, :sap_hana,
  hostname: "achanaproxy.mpi-sws.org",
  port: 39013,
  username: "acdev",
  password: "Aircloakdev1",
  database: "SYSTEMDB"

# Using 0 timeout in tests, since due to the log level, log message won't be captured anyway.
config :cloak, :flush_query_log_timeout, 0

# using connection retries in tests, since there are occasional failures with SAP HANA
config :cloak, connect_retries: 10, connect_retry_delay: 500
