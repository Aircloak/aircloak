use Mix.Config

# Print only errors during test
config :logger, level: :error
config :lager, handlers: [{LagerLogger, [level: :error]}]

config :cloak, :cloak_db, idle_timeout: 100
