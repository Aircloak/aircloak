# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Turn off sasl error logger. Crashes are usually logged through standard OTP error reports, so SASL only
# adds to the noise.
config :sasl, :sasl_error_logger, false

config :aircloak_common, :env, Mix.env()

# Configures the endpoint
config :air, AirWeb.Endpoint,
  check_origin: false,
  http: [port: 8080],
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub_server: AirWeb.PubSub,
  live_view: [
    signing_salt: "xHLT7Pf+zED1w1yKvqav/q77bf9qi/z3"
  ]

config :air, AirWeb.MonitoringEndpoint,
  check_origin: false,
  http: [port: 8081],
  render_errors: [accepts: ~w(json)]

config :air, :https_port, 8443

# Configures Elixir's Logger
config :logger, :console,
  format: "$date $time $metadata[$level] $levelpad$message\n",
  metadata: [:request_id]

# Configure phoenix generators
config :phoenix, :generators,
  migration: true,
  binary_id: false

config :phoenix, :json_library, Jason

# configure markdown compiler
config :phoenix, :template_engines, md: Air.Phoenix.MarkdownEngine

config :air, Air.Repo,
  pool_size: 20,
  loggers: [Ecto.LogEntry]

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM,
  bom_file: "priv/bom.json.example",
  dependencies: "priv/dependencies.zip.example"

config :air, Air.PsqlServer,
  port: 8432,
  detailed_log: true

config :air, logs_retention_days: 1

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"

config :air, diffix_version: "Dogwood"

if File.exists?("config/#{Mix.env()}.local.exs") do
  import_config "#{Mix.env()}.local.exs"
end
