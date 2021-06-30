use Mix.Config

config :air, :deploy_config_file, "config.json"

config :air, AirWeb.Endpoint, cache_static_manifest: "priv/static/cache_manifest.json"

# Do not print debug messages in production
config :logger, level: :info

# ## SSL Support
#
# To get SSL working, you will need to add the `https` key
# to the previous section and set your `:url` port to 443:
#
#     config :air, AirWeb.Endpoint,
#       ...
#       url: [host: "example.com", port: 443],
#       https: [port: 443,
#               keyfile: System.get_env("SOME_APP_SSL_KEY_PATH"),
#               certfile: System.get_env("SOME_APP_SSL_CERT_PATH")]
#
# Where those two env variables return an absolute path to
# the key and cert in disk or a relative path inside priv,
# for example "priv/ssl/server.key".
#
# We also recommend setting `force_ssl`, ensuring no data is
# ever sent via http, always redirecting to https:
#
#     config :air, AirWeb.Endpoint,
#       force_ssl: [hsts: true]
#
# Check `Plug.SSL` for all available options in `force_ssl`.

# ## Using releases
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start the server for all endpoints:
#
#     config :phoenix, :serve_endpoints, true
#
# Alternatively, you can configure exactly which server to
# start per endpoint:
#
#     config :air, AirWeb.Endpoint, server: true
#
# You will also need to set the application root to `.` in order
# for the new static assets to be served after a hot upgrade:
#
#     config :air, AirWeb.Endpoint, root: "."

config :phoenix, :serve_endpoints, true

config :air, Air.BOM,
  bom_file: "priv/bom.json",
  dependencies: "priv/dependencies.zip"

# Where to expect the Central endpoint to be located
config :air, :central,
  central_site: "wss://central.aircloak.com",
  min_reconnect_interval: :timer.seconds(10),
  max_reconnect_interval: :timer.minutes(5)

config :air, logs_retention_days: 15
