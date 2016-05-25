use Mix.Config

config :logger,
  console: [
    format: {Cloak.Logger.DevFormatter, :format},
    metadata: [:file_name, :line_no, :log_level, :file, :line]
  ]

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  socket_url: "wss://insights.air-local:20000/cloak/socket/websocket",
  # The reconnect intervals start out very low, and doubles for each failed attempt until
  # they arrive at and stay at this max interval.
  min_reconnect_interval: :timer.seconds(30),
  max_reconnect_interval: :timer.minutes(3)

config :cloak, :in_development, true

config :cloak, data_sources: [
  local: [
    driver: Cloak.DataSource.PostgreSQL,
    parameters: [
      hostname: "localhost",
      username: "cloak",
      database: "cloak",
      sync_connect: true,
      pool_size: 4
    ],
    tables: [
      test: [
        name: "user_data",
        user_id: "user_id",
        ignore_unsupported_types: false
      ],
      purchases: [
        name: "purchases",
        user_id: "uid",
        ignore_unsupported_types: false
      ]
    ]
  ]
]
