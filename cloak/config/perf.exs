use Mix.Config

config :cloak, :deploy_config_file, "perf.json"

# Print only errors during test
config :logger, level: :error

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  air_site: "ws://127.0.0.1:29876",
  min_reconnect_interval: 50,
  max_reconnect_interval: 50,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.Json

config :cloak, :sanitize_otp_errors, false
