# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :aircloak_common, :env, Mix.env

# Turn off sasl error logger. Do not change this, because data privacy might be compromised.
config :sasl, :sasl_error_logger, false

config :logger,
  level: :info,
  backends: [:console],
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

config :cloak, :air,
  serializer: Air.CloakSocketSerializer

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

  # The minimum count of dropped outliers.
  min_outliers_count: 2,

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {3, 0.5},

  # The standard deviation for the noisy value of max(top_average, 2 * global_average) added to summed values.
  sum_noise_sigma: 1

config :cloak, :in_development, false

config :cloak, :data_source,
  timeout: :timer.hours(12),
  batch_size: 50_000

import_config "#{Mix.env}.exs"
if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
