use Mix.Config

config :logger,
  level: :warn,
  console: [
    format: "$metadata[$level] $message\n",
    metadata: [:request_id]
  ]
