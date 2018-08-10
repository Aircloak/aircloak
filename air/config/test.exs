use Mix.Config

config :air, :deploy_config_file, "test.json"

config :air, AirWeb.Endpoint,
  http: [port: 8082],
  server: true

config :air, :https_port, 8444

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :air, Air.Repo, pool: Ecto.Adapters.SQL.Sandbox, ownership_timeout: :timer.minutes(1)

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1

config :air, Air.PsqlServer, port: 8433

config :air, :auto_aircloak_export, false

# Unit tests register multiple datasources with random names which leads to explosion of shadow databases.
# Since we don't need shadow dbs in unit tests (they are used only in integration tests), we're disabling them
config :air, :shadow_db?, false
