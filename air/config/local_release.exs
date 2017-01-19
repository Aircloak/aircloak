use Mix.Config

import_config "config.exs"

config :air, :deploy_config_file, "dev.json"

config :air, :central,
  central_site: "ws://localhost:7080",
  min_reconnect_interval: 1000,
  max_reconnect_interval: 50000

config :air, Air.BOM,
  location: "priv/bom.json.example"

config :air, Air.PsqlServer,
  port: 8432
