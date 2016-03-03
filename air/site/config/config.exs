# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :air, Air.Endpoint,
  url: [host: "localhost"],
  root: Path.dirname(__DIR__),
  secret_key_base: "7nOn9WDwUZLjXnQah8+ocSF0r/YhYpDp0hSw4FE6Zq+Ic2TVYcDCQe+2MT8wqD/R",
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Configure phoenix generators
config :phoenix, :generators,
  migration: true,
  binary_id: false

config :guardian, Guardian,
  allowed_algos: ["HS512"],
  verify_module: Guardian.JWT,
  issuer: "Aircloak Air",
  ttl: { 30, :days },
  verify_issuer: true,
  secret_key: "6MvtANFkxCr3VaDY/C8oCooF6Pg1uqFzOWNYVMry/V5acmSPuQydPeU5X5Jh",
  serializer: Air.GuardianSerializer

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
