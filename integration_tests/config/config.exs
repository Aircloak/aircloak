# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :aircloak_common, :env, Mix.env

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
  low_count_soft_lower_bound: {4, 0.6},

  # The minimum absolute value of the noisy lower bound.
  # Reported values equal or exceed this value.
  low_count_absolute_lower_bound: 2,

  # The mean and standard deviation for the count of outliers dropped, from the top of the collection,
  # during the anonymized aggregation of values. The outliers are replaced with the noisy
  # average value for the top of the remaining users in the collection.
  outliers_count: {4, 1},

  # The minimum count of dropped outliers.
  min_outliers_count: 1,

  # The mean and standard deviation for the count of items at the top of a collection,
  # used for computing the average value of the top.
  top_count: {5, 1},

  # The standard deviation for the noisy top average added to summed values.
  sum_noise_sigma: 2

config :cloak, :in_development, false

config :aircloak_common, :env, Mix.env

# Configures the endpoint
config :air, Air.Endpoint,
  check_origin: false,
  http: [port: 8080],
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :air, :https_port, 8443

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Configure phoenix generators
config :phoenix, :generators,
  migration: true,
  binary_id: false

# configure markdown compiler
config :phoenix, :template_engines, md: Air.Phoenix.MarkdownEngine

config :guardian, Guardian,
  allowed_algos: ["HS512"],
  verify_module: Guardian.JWT,
  issuer: "Aircloak Air",
  ttl: { 30, :days },
  verify_issuer: true,
  serializer: Air.GuardianSerializer

config :scrivener_html,
  routes_helper: Air.Router.Helpers

config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 10,
  # We need it to work with `pgbouncer` (see https://github.com/elixir-ecto/postgrex#pgbouncer)
  prepare: :unnamed

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM,
  location: "priv/bom.json.example"

config :air, Air.PsqlServer,
  port: 8432

config :air, :central,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson

import_config "#{Mix.env}.exs"
