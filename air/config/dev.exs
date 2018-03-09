use Mix.Config

config :air, :deploy_config_file, "dev.json"

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :air, AirWeb.Endpoint,
  debug_errors: true,
  code_reloader: true,
  watchers: [node: ["node_modules/brunch/bin/brunch", "watch", "--stdin",
    cd: Path.expand("../assets", __DIR__)]]

# Watch static and templates for browser reloading.
config :air, AirWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$},
      ~r{priv/gettext/.*(po)$},
      ~r{lib/air_web/views/.*(ex)$},
      ~r{lib/air_web/templates/.*(eex)$}
    ]
  ]

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development.
# Do not configure such in production as keeping
# and calculating stacktraces is usually expensive.
config :phoenix, :stacktrace_depth, 20

# Where to expect the Central endpoint to be located
config :air, :central,
  central_site: "ws://localhost:7080",
  min_reconnect_interval: 1000,
  max_reconnect_interval: 1000

# Use smaller pools in dev for endpoints and repo. We shouldn't issue a huge load in dev mode anyway, and
# less processes makes the supervision tree view in observer nicer.
config :air, AirWeb.Endpoint,
  http: [acceptors: 2],
  https: [acceptors: 2]

config :air, AirWeb.MonitoringEndpoint,
  http: [acceptors: 2]

config :air, Air.PsqlServer,
  ranch_opts: [num_acceptors: 2]

config :air, Air.Repo,
  pool_size: 2,
  loggers: []

config :air, :auto_aircloak_export, true

config :air, :license, public_key: "priv/mock_key_public.pem"
