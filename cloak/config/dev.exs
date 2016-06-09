use Mix.Config

config :cloak, :api, address: '0.0.0.0'

config :cloak, :air,
  # The reconnect intervals start out very low, and doubles for each failed attempt until
  # they arrive at and stay at this max interval.
  min_reconnect_interval: :timer.seconds(30),
  max_reconnect_interval: :timer.minutes(3)

config :cloak, :in_development, true
