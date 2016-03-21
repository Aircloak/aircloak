use Mix.Config

config :logger, console: [
  format: {Cloak.Logger.ProdFormatter, :format},
  metadata: [:log_level, :real_message]
]

config :cloak, :api, address: '127.0.0.1'

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.aircloak.com/results'
