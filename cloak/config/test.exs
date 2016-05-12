use Mix.Config

# Print only errors during test
config :logger,
  level: :error,
  console: [
    format: {Cloak.Logger.DevFormatter, :format},
    metadata: [:file_name, :line_no, :log_level]
  ]

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'

config :cloak, data_sources: [
  local: [
    driver: Cloak.DataSource.PostgreSQL,
    parameters: [
      host: "127.0.0.1",
      username: "postgres",
      database: "cloaktest1",
      sync_connect: true,
      pool_size: 2
    ],
    tables: [
    ]
  ]
]

config :cloak, :air,
  socket_url: "ws://127.0.0.1:29876/cloak/socket/websocket",
  max_reconnect_interval: :timer.minutes(1),
  max_rejoin_interval: :timer.minutes(1),
  serializer: Phoenix.Channels.GenSocketClient.Serializer.Json
