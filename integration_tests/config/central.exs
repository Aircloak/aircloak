use Mix.Config

config :central, :deploy_config_file, "test.json"

config :central, CentralWeb.Endpoint,
  check_origin: false,
  http: [port: 7081],
  server: true,
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: CentralWeb.PubSub, adapter: Phoenix.PubSub.PG2]

config :central, Central.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 20,
  prepare: :unnamed

config :central, :https_port, 7444

config :central, air_status_logging_interval: :timer.seconds(10)

config :central, :delete_air_rpcs_after, :timer.hours(24) * 7

config :central, :simulate_elastic?, true

config :central, :license,
  public_key: "priv/mock_key_public.pem",
  private_key: "priv/mock_key_private.pem"
