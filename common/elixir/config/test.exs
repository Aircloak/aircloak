use Mix.Config

config :logger,
  level: :warn,
  console: [
    format: "$metadata[$level] $message\n",
    metadata: [:request_id]
  ]

# Make tests related to hashing run faster
config :comeonin, :pbkdf2_rounds, 1
