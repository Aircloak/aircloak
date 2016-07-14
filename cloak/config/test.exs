use Mix.Config

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
  socket_url: "ws://127.0.0.1:29876/cloak/socket/websocket",
  min_reconnect_interval: 50,
  max_reconnect_interval: :timer.seconds(5),
  serializer: Phoenix.Channels.GenSocketClient.Serializer.Json

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  count_soft_lower_bound: {5, 0},

  # The minimum absolute value of the noisy lower bound.
  count_absolute_lower_bound: 2,

  # The number of outliers dropped, from top and bottom, during the anonymized aggregation of values.
  # The outliers are replaced with the average value for that margin of the collection.
  dropped_outliers_count: 1,

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {5, 0},

  # The standard deviation for the noisy top average added to summed values.
  sum_noise_sigma: 0
