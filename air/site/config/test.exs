use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :air, Air.Endpoint,
  http: [port: Air.EnvSettings.tcp_port("insights/http")],
  server: true,
  secret_key_base: "abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234"

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :air, Air.Repo, pool: Ecto.Adapters.SQL.Sandbox

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1
