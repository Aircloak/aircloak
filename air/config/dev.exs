use Mix.Config

config :air, :deploy_config_file, "dev.json"

config :air, AirWeb.Endpoint,
  debug_errors: true,
  code_reloader: true,
  watchers: [
    yarn: [
      "watch",
      cd: Path.expand("../assets", __DIR__)
    ]
  ]

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
config :logger, :console, format: "[$level] $levelpad$message\n"

# Set a higher stacktrace during development.
# Do not configure such in production as keeping
# and calculating stacktraces is usually expensive.
config :phoenix, :stacktrace_depth, 20

# Use smaller pools in dev for endpoints and repo. We shouldn't issue a huge load in dev mode anyway, and
# less processes makes the supervision tree view in observer easier to read.
config :air, AirWeb.Endpoint,
  http: [transport_options: [num_acceptors: 2]],
  https: [transport_options: [num_acceptors: 2]]

config :air, AirWeb.MonitoringEndpoint, http: [transport_options: [num_acceptors: 2]]

config :air, Air.Repo,
  pool_size: 2,
  loggers: []
