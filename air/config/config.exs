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
config :air, AirWeb.Endpoint,
  check_origin: false,
  http: [port: 8080],
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :air, Air.Service.Version,
  version_expiry: File.read!("../RELEASE_EXPIRY_DATE") |> String.trim() |> Date.from_iso8601!()

config :air, AirWeb.MonitoringEndpoint,
  check_origin: false,
  http: [port: 8081],
  render_errors: [accepts: ~w(json)]

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
  routes_helper: AirWeb.Router.Helpers

config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 10,
  # We need it to work with `pgbouncer` (see https://github.com/elixir-ecto/postgrex#pgbouncer)
  prepare: :unnamed

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM,
  bom_file: "priv/bom.json.example",
  dependencies: "priv/dependencies.zip.example"

config :air, Air.PsqlServer,
  port: 8432,
  detailed_log: true

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

config :air, :central,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson,
  call_timeout: :timer.seconds(5)

config :air, :central_queue,
  retry_delay: :timer.minutes(1),
  max_size: 1000

if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
