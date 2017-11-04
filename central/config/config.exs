# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Turn off sasl error logger. Crashes are usually logged through standard OTP error reports, so SASL only
# adds to the noise.
config :sasl, :sasl_error_logger, false

config :aircloak_common, :env, Mix.env

# Configures the endpoint
config :central, CentralWeb.Endpoint,
  check_origin: false,
  http: [port: 7080],
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: CentralWeb.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :central, :https_port, 7443

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $metadata $message\n",
  metadata: [:request_id, :customer, :air]

# Configure phoenix generators
config :phoenix, :generators,
  migration: true,
  binary_id: false

# configure markdown compiler
config :phoenix, :template_engines, md: Central.Phoenix.MarkdownEngine

config :guardian, Guardian,
  allowed_algos: ["HS512"],
  verify_module: Guardian.JWT,
  issuer: "Aircloak Central",
  ttl: { 30, :days },
  verify_issuer: true,
  serializer: Central.GuardianSerializer

config :central, Central.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 10,
  # We need it to work with `pgbouncer` (see https://github.com/elixir-ecto/postgrex#pgbouncer)
  prepare: :unnamed

config :central, ecto_repos: [Central.Repo]

config :central, air_status_logging_interval: :timer.seconds(10)

config :central, :delete_air_rpcs_after, :timer.hours(24) * 7

config :quantum, central: [
  cron: [
    "@daily": {Central.Service.Customer, :delete_old_rpcs},
  ]
]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
