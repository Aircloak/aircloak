use Mix.Config

config :central, :deploy_config_file, "test.json"

config :central, CentralWeb.Endpoint,
  http: [port: 7081],
  server: true

config :central, :https_port, 7444

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :central, Central.Repo, pool: Ecto.Adapters.SQL.Sandbox

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1

config :central, :license, private_key: "priv/mock_key_private.pem"
