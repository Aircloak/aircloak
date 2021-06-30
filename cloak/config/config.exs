# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :aircloak_common, :env, Mix.env()

# Turn off sasl error logger. Do not change this, because data privacy might be compromised.
config :sasl, :sasl_error_logger, false

config :logger,
  level: :info,
  truncate: 65536,
  console: [
    format: "$date $time $metadata[$level] $levelpad$message\n",
    metadata: [:query_id]
  ]

config :cloak, :api, port: 8098

config :cloak, :air, serializer: Air.CloakSocketSerializer

config :cloak, :anonymizer,
  # The mean and standard deviation for the lower bound of the number
  # of users that must be in a bucket to get reported.
  low_count_soft_lower_bound: {4, 0.5},

  # The minimum absolute value of the noisy lower bound.
  # Reported values equal or exceed this value.
  low_count_absolute_lower_bound: 2,

  # The range and standard deviation for the count of outliers dropped, from the top of the collection,
  # during the anonymized aggregation of values. The outliers are replaced with the noisy
  # average value for the top of the remaining users in the collection.
  outliers_count: {1, 2, 0.5},

  # The range and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {1, 3, 0.5},

  # The base sigma value used for sum and count calculations.
  # This base sigma is scaled based on the average values and the `sum_noise_sigma_scale_params`.
  sum_noise_sigma: 1,

  # `sum_noise_sigma` is scaled based on the values being anonymized following the following formula:
  #   max(average_factor * average, top_average_factor * top_average)
  # The parameter takes the form: {average_factor, top_average_factor}
  sum_noise_sigma_scale_params: {1, 0.5},

  # The mean and standard deviation for the lower bound of the number of users that have
  # to be in a bucket so that statistics-based aggregators will compute a result.
  stats_aggregators_soft_lower_bound: {10, 0.5},

  # This value specifies the isolating factor threshold. See `Isolating columns` in anonymization.md.
  isolating_column_threshold: 0.8

# These values specify how many users are needed to establish the min/max bound for a given column. See `Overflow
# protection and bound analysis` in anonymization.md.
config :cloak, :bound_size_cutoff,
  min: 10,
  mean: 20,
  std_dev: 5

config :cloak, :shadow_tables,
  # Number of allowed negative conditions with rare values. See `Shadow tables` in anonymization.md.
  max_rare_negative_conditions: 0,

  # Number of popular values to keep in a shadow table
  size: 200,

  # Minimum users with a particular value needed to include it in a shadow table
  min_users: 10

config :cloak, :in_development, false

config :cloak, :data_source,
  timeout: :timer.hours(12),
  batch_size: 25_000,
  connect_timeout: :timer.seconds(5),
  connection_keep_time: :timer.minutes(1)

config :cloak, :flush_query_log_timeout, 500

import_config "#{Mix.env()}.exs"

if File.exists?("config/#{Mix.env()}.local.exs") do
  import_config "#{Mix.env()}.local.exs"
end
