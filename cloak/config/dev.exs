use Mix.Config

config :cloak, :deploy_config_file, "dev.json"

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  # The reconnect intervals start out very low, and doubles for each failed attempt until
  # they arrive at and stay at this max interval.
  min_reconnect_interval: 100,
  max_reconnect_interval: 100

config :cloak, :in_development, true
config :cloak, :sanitize_otp_errors, false
