use Mix.Config

# Print only errors during test
config :logger, level: :error
config :lager, handlers: [{LagerLogger, [level: :error]}]

config :cloak, :cloak_db,
  connection: [
    host: "127.0.0.1",
    user: "postgres",
    database: "cloaktest1",
    port: 5432
  ],
  idle_timeout: 100
