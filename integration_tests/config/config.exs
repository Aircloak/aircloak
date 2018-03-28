use Mix.Config

if Mix.env() != :test,
  do: Mix.raise("This project is meant to be used only in test env. Run your mix task with `MIX_ENV=test mix ...`")

config :sasl, :sasl_error_logger, false

config :logger,
  level: :error,
  backends: [:console],
  console: [
    format: "$time [$level] $metadata$message\n",
    metadata: [:query_id]
  ]

config :comeonin, :pbkdf2_rounds, 1

config :phoenix, :template_engines, md: Air.Phoenix.MarkdownEngine

config :aircloak_common, :env, Mix.env()

import_config "air.exs"
import_config "cloak.exs"
import_config "central.exs"
