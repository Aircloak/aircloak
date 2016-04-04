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
    driver: DataSource.PostgreSQL,
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
