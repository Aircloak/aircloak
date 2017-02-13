use Mix.Config

import_config "config.exs"
config :cloak, :deploy_config_file, "dev.json"
config :cloak, :sanitize_otp_errors, true
