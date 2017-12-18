use Mix.Config

config :logger,
  level: :info,
  backends: [:console],
  console: [format: "$time [$level] $metadata$message\n"]

queue_spec = fn(opts) ->
  [
    max_time: Keyword.fetch!(opts, :max_waiting_time),
    regulators: [counter: [limit: Keyword.fetch!(opts, :concurrency)]]
  ]
end

config :aircloak_ci, :queues,
  [
    docker_build: queue_spec.(concurrency: 1, max_waiting_time: :timer.hours(1)),
    compile: queue_spec.(concurrency: 5, max_waiting_time: :timer.hours(1)),
    test: queue_spec.(concurrency: 20, max_waiting_time: :timer.hours(1)),
    compliance: queue_spec.(concurrency: 1, max_waiting_time: :timer.hours(1))
  ]

import_config "#{Mix.env}.exs"
if File.exists?("config/#{Mix.env}.local.exs") do
  import_config "#{Mix.env}.local.exs"
end
