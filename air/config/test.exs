use Mix.Config

config :air, :deploy_config_file, "test.json"

config :air, AirWeb.Endpoint,
  http: [port: 8082],
  server: true

config :air, :https_port, 8444

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :air, Air.Repo, pool: Ecto.Adapters.SQL.Sandbox

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1

config :air, Air.PsqlServer, port: 8433

config :air, :auto_aircloak_export, false
