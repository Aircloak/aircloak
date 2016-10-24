use Mix.Config

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :air, Air.Endpoint,
  debug_errors: true,
  code_reloader: true,
  watchers: [node: ["node_modules/brunch/bin/brunch", "watch", "--stdin",
    cd: Path.expand("../", __DIR__)]]

# Watch static and templates for browser reloading.
config :air, Air.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$},
      ~r{priv/gettext/.*(po)$},
      ~r{web/views/.*(ex)$},
      ~r{web/templates/.*(eex)$}
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
  min_reconnect_interval: 100,
  max_reconnect_interval: 30000
