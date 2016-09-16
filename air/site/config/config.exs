# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

Code.require_file("config/env_settings.exs")

# Configures the endpoint
config :air, Air.Endpoint,
  url: [host: "localhost"],
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

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

config :air, :etcd_port, Air.EnvSettings.tcp_port("etcd/client")

config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 10,
  # We need it to work with `pgbouncer` (see https://github.com/elixir-ecto/postgrex#pgbouncer)
  prepare: :unnamed

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM,
  location: "priv/bom.json.example"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
