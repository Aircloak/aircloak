use Mix.Config

config :air, :deploy_config_file, "test.json"

config :guardian, Guardian,
  allowed_algos: ["HS512"],
  verify_module: Guardian.JWT,
  issuer: "Aircloak Air",
  ttl: { 30, :days },
  verify_issuer: true,
  serializer: Air.GuardianSerializer

config :air, Air.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 20,
  prepare: :unnamed

config :air, AirWeb.Endpoint,
  check_origin: false,
  http: [port: 8081],
  server: true,
  root: Path.dirname(__DIR__),
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Air.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :air, Air.Service.Version,
  version_expiry: ~D[2100-01-01]

config :air, :https_port, 8444

config :air, Air.PsqlServer, port: 8433

config :air, ecto_repos: [Air.Repo]

config :air, Air.BOM,
  bom_file: "priv/bom.json.example",
  dependencies: "priv/dependencies.zip.example"

config :air, :central,
  serializer: Phoenix.Channels.GenSocketClient.Serializer.GzipJson,
  central_site: "ws://localhost:7081",
  min_reconnect_interval: 1000,
  max_reconnect_interval: 50000,
  call_timeout: 100

config :air, :usage_report_interval, 100

config :air, :auto_aircloak_export, true

config :air, :central_queue,
  retry_delay: 1,
  max_size: 1000

config :air, :psql_tcp_allowed, false
