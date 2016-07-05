# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# turn off sasl error logger
config :sasl, :sasl_error_logger, false

config :logger,
  level: :info,
  backends: [:console],
  console: [
    format: {Cloak.Logger.Formatter, :format},
    metadata: [:file_name, :line_no, :log_level, :file, :line]
  ]

config :kernel,
  # The portnumber the cloak erlang vm listens
  # to for communication from other nodes.
  inet_dist_listen_min: 34000,
  inet_dist_listen_max: 35000

config :cloak, :api, port: 8098

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.aircloak.com/results',
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  count_soft_lower_bound: {5, 1},

  # The minimum absolute value of the noisy lower bound.
  count_absolute_lower_bound: 2,

  # The number of outliers dropped, from top and bottom, during the anonymized aggregation of values.
  # The outliers are replaced with the average value for that margin of the collection.
  dropped_outliers_count: 1,

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {5, 1},

  # The standard deviation for the noisy top average added to summed values.
  sum_noise_sigma: 2.0

config :cloak, :in_development, false


import_config "#{Mix.env}.exs"
if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
