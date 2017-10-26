use Mix.Config

config :central, :deploy_config_file, "dev.json"

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :central, Central.Endpoint,
  debug_errors: true,
  code_reloader: true,
  watchers: [node: ["node_modules/brunch/bin/brunch", "watch", "--stdin",
    cd: Path.expand("../", __DIR__)]]

# Watch static and templates for browser reloading.
config :central, Central.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$},
      ~r{priv/gettext/.*(po)$},
      ~r{lib/central_web/views/.*(ex)$},
      ~r{lib/central_web/templates/.*(eex)$}
    ]
  ]

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $metadata $message\n", metadata: [:customer, :air]

# Set a higher stacktrace during development.
# Do not configure such in production as keeping
# and calculating stacktraces is usually expensive.
config :phoenix, :stacktrace_depth, 20
