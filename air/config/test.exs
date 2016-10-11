use Mix.Config

config :air, Air.Endpoint,
  http: [port: 8081],
  server: true

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :air, Air.Repo, pool: Ecto.Adapters.SQL.Sandbox

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1

config :air, Air.PsqlServer,
  port: 8433
