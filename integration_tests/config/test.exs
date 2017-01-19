use Mix.Config

config :air, :deploy_config_file, "test.json"
config :cloak, :deploy_config_file, "test.json"

# Print only errors during test
config :logger,
  level: :error,
  console: [
    format: {Cloak.Logger.Formatter, :format},
    metadata: [:file_name, :line_no, :log_level]
  ]

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  air_site: "wss://127.0.0.1:8444",
  min_reconnect_interval: 50,
  max_reconnect_interval: 50,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  low_count_soft_lower_bound: {5, 0},

  # The minimum absolute value of the noisy lower bound.
  low_count_absolute_lower_bound: 2,

  # The mean and standard deviation for the count of outliers dropped, from the top of the collection,
  # during the anonymized aggregation of values. The outliers are replaced with the noisy
  # average value for the top of the remaining users in the collection.
  outliers_count: {4, 0},

  # The minimum count of dropped outliers.
  min_outliers_count: 1,

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {5, 0},

  # The standard deviation for the noisy top average added to summed values.
  sum_noise_sigma: 0


config :air, Air.Endpoint,
  http: [port: 8081],
  server: true

config :air, :https_port, 8444

# Configure your database
config :air, Air.Repo, pool: Ecto.Adapters.SQL.Sandbox

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1

config :air, Air.PsqlServer,
  port: 8433

config :air, :central,
  central_site: "ws://localhost:7080",
  min_reconnect_interval: 1000,
  max_reconnect_interval: 50000
