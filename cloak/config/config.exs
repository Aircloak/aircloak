# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :aircloak_common, :env, Mix.env()

# Turn off sasl error logger. Do not change this, because data privacy might be compromised.
config :sasl, :sasl_error_logger, false

config :logger,
  level: :info,
  backends: [:console, Cloak.Query.LogCollector],
  console: [
    format: "$time [$level] $metadata$message\n",
    metadata: [:query_id]
  ]

config :kernel,
  # The portnumber the cloak erlang vm listens
  # to for communication from other nodes.
  inet_dist_listen_min: 34000,
  inet_dist_listen_max: 35000

config :cloak, :api, port: 8098

config :cloak, :air, serializer: Air.CloakSocketSerializer

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  low_count_soft_lower_bound: {4, 0.5},

  # The minimum absolute value of the noisy lower bound.
  # Reported values equal or exceed this value.
  low_count_absolute_lower_bound: 2,

  # The mean and standard deviation for the count of outliers dropped, from the top of the collection,
  # during the anonymized aggregation of values. The outliers are replaced with the noisy
  # average value for the top of the remaining users in the collection.
  outliers_count: {3, 0.5},

  # The minimum and maximum amounts of dropped outliers or top values.
  group_limits: {2, 6},

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {3, 0.5},

  # The base sigma value used for sum and count calculations.
  # This base sigma is scaled based on the average values and the `sum_noise_sigma_scale_params`.
  sum_noise_sigma: 1,

  # `sum_noise_sigma` is scaled based on the values being anonymized following the following formula:
  #
  #   max(
  #     lower_bound,
  #     max(average_factor * average, top_average_factor * top_average)
  #   )
  #
  # The parameter takes the form:
  # {lower_bound, average_factor, top_average_factor}
  sum_noise_sigma_scale_params: {2, 1, 0.5}

config :cloak, :in_development, false

config :cloak, :data_source,
  timeout: :timer.hours(12),
  batch_size: 25_000,
  connect_timeout: :timer.seconds(5),
  connection_keep_time: :timer.minutes(1)

config :cloak, :flush_query_log_timeout, 500

config :cloak, Cloak.Scheduler,
  jobs: [
    {"* * * * *", {Cloak.DataSource.SerializingUpdater, :run_liveness_check, []}}
  ]

import_config "#{Mix.env()}.exs"

if File.exists?("config/#{Mix.env()}.local.exs") do
  import_config "#{Mix.env()}.local.exs"
end
