use Mix.Config

config :central, :deploy_config_file, "test.json"

config :central, Central.Endpoint,
  check_origin: false,
  http: [port: 7081],
  server: true,
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Central.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :central, Central.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 20,
  prepare: :unnamed

config :central, :https_port, 7444

config :central, air_status_logging_interval: :timer.seconds(10)
