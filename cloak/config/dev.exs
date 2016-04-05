use Mix.Config

config :logger,
  console: [
    format: {Cloak.Logger.DevFormatter, :format},
    metadata: [:file_name, :line_no, :log_level, :file, :line]
  ]

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.air-local:20000',
  socket_url: "wss://insights.air-local:20000/cloak/socket/websocket",
  reconnect_interval: :timer.minutes(1),
  rejoin_interval: :timer.minutes(1)

config :cloak, :in_development, true

config :cloak, data_sources: [
  local: [
    driver: Cloak.DataSource.PostgreSQL,
    parameters: [
      host: "localhost",
      username: "cloak",
      database: "cloak",
      sync_connect: true,
      pool_size: 4
    ],
    tables: [
      test: [
        name: "user_data",
        user_id: "user_id",
        row_id: "id"
      ]
    ]
  ]
]
