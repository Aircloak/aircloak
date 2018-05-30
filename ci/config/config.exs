use Mix.Config

config :logger,
  level: :info,
  backends: [:console],
  console: [format: "$time [$level] $metadata$message\n"]

regulator = fn
  {:concurrent, limit} -> [counter: [limit: limit]]
  {:per_second, limit} -> [rate: [limit: limit]]
end

queue_spec = fn spec ->
  {limit, opts} = Keyword.pop(spec, :limit)
  [max_time: Keyword.get(opts, :max_time, :timer.hours(1)), regulators: regulator.(limit)]
end

config :aircloak_ci, :queues,
  docker_build: queue_spec.(limit: {:concurrent, 1}),
  compile: queue_spec.(limit: {:concurrent, 10}),
  test: queue_spec.(limit: {:concurrent, 10}),
  compliance: queue_spec.(limit: {:concurrent, 1}),
  system_test: queue_spec.(limit: {:concurrent, 1}),
  github_api:
    (if Mix.env() == :test do
       queue_spec.(limit: {:concurrent, 1000})
     else
       queue_spec.(limit: {:per_second, 1})
     end),
  job: queue_spec.(limit: {:concurrent, 10})

import_config "#{Mix.env()}.exs"

if File.exists?("config/#{Mix.env()}.local.exs") do
  import_config "#{Mix.env()}.local.exs"
end
