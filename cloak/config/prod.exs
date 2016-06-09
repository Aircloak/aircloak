use Mix.Config

config :logger, console: [
  format: {Cloak.Logger.ProdFormatter, :format},
  metadata: [:log_level, :real_message]
]

config :cloak, :api, address: '127.0.0.1'

config :cloak, :air,
  socket_url: "wss://insights.aircloak.com/cloak/socket/websocket",
  # The reconnect intervals start out very low, and doubles for each failed attempt until
  # they arrive at and stay at this max interval.
  min_reconnect_interval: :timer.seconds(10),
  max_reconnect_interval: :timer.minutes(1)
