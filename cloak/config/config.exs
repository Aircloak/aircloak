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

config :cloak, :noise,
  # The minimum number of users that must be in a bucket to get reported.
  absolute_lower_bound: 2,

  # After adding noise with standard deviation sigma_soft_lower_bound to the number of users in a bucket
  # this noisy count has to be greater than soft_lower_bound to get reported.
  sigma_soft_lower_bound: 1,
  soft_lower_bound: 5,

  # min_sigma: The minimum standard deviation allowed for noise.
  # max_sigma: The maximum total standard deviation to be used. This can be exceeded without
  # causing privacy issues, but is honoured to improve the usability of the system.
  min_sigma: 2,
  max_sigma: 20,

  # The anonymized results contain a layer of noise that is constant and unique to
  # the bucket. The noise is normal with a certain standard deviation.
  constant_noise_sd: 1.5

config :cloak, :in_development, false


import_config "#{Mix.env}.exs"
if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
