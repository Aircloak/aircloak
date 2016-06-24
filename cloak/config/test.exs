use Mix.Config

# Print only errors during test
config :logger,
  level: :error,
  console: [
    format: {Cloak.Logger.Formatter, :format},
    metadata: [:file_name, :line_no, :log_level]
  ]

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  socket_url: "ws://127.0.0.1:29876/cloak/socket/websocket",
  min_reconnect_interval: 50,
  max_reconnect_interval: :timer.seconds(5),
  serializer: Phoenix.Channels.GenSocketClient.Serializer.Json

config :cloak, :anonymizer, sum_noise_sigma: 0
