use Mix.Config

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.air-local:20000'

config :cloak, :in_development, true
