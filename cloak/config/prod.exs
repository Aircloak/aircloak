use Mix.Config

config :cloak, :api, address: '127.0.0.1'

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.aircloak.com/results'
