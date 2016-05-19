use Mix.Config

config :logger, console: [
  format: {Cloak.Logger.ProdFormatter, :format},
  metadata: [:log_level, :real_message]
]

config :cloak, :api, address: '127.0.0.1'
