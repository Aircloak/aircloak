use Mix.Config

config :cloak, :deploy_config_file, "test.json"

config :cloak, :api, port: 8098, address: '0.0.0.0'

config :cloak, :in_development, false

config :cloak, :alarm_handler, install: false

config :cloak, :air,
  air_site: "wss://127.0.0.1:8444",
  min_reconnect_interval: 50,
  max_reconnect_interval: 50,
  return_url: 'https://infrastructure-api.aircloak.com/results',
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson

config :cloak, :anonymizer,
  low_count_soft_lower_bound: {5, 0},
  low_count_absolute_lower_bound: 2,
  outliers_count: {4, 0},
  min_outliers_count: 1,
  top_count: {5, 0},
  sum_noise_sigma: 0
