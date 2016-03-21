use Mix.Config

# Print only errors during test
config :logger,
  level: :error,
  console: [
    format: {Cloak.Logger.DevFormatter, :format},
    metadata: [:file_name, :line_no, :log_level]
  ]

config :cloak, :cloak_db,
  connection: [
    host: "127.0.0.1",
    user: "postgres",
    database: "cloaktest1",
    port: 5432
  ],
  idle_timeout: 100

config :cloak, :alarm_handler, install: false

config :cloak, :api, address: '0.0.0.0'
