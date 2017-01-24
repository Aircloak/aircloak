use Mix.Config

config :air, :deploy_config_file, "test.json"

config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 20,
  prepare: :unnamed

config :air, Air.Endpoint,
  check_origin: false,
  http: [port: 8081],
  server: true,
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :air, :https_port, 8444

config :air, Air.PsqlServer, port: 8433

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM, location: "priv/bom.json.example"

config :air, :central,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson,
  central_site: "ws://localhost:7081",
  min_reconnect_interval: 1000,
  max_reconnect_interval: 50000
