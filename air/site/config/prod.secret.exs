use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
config :air, Air.Endpoint,
  secret_key_base: "dADRwH9d94AWPaFHxrWIJlLToFzplfuOJa6FNwHrJIcswzl5srQ9yhxQufVaTrRq"

# Configure your database
config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "air_prod",
  pool_size: 20
