use Mix.Config

import_config "config.exs"

config :air, :deploy_config_file, "dev.json"

config :air, Air.BOM,
  bom_file: "priv/bom.json.example",
  dependencies: "priv/dependencies.zip.example"

config :air, Air.PsqlServer, port: 8432
