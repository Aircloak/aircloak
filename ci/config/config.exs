use Mix.Config

config :logger,
  level: :info,
  backends: [:console],
  console: [format: "$time [$level] $metadata$message\n"]

import_config "#{Mix.env}.exs"
if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
