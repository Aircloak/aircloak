ExUnit.start(exclude: [:disabled, :pending])

Mix.Task.run("ecto.create", ~w(-r Air.Repo --quiet))
Mix.Task.run("ecto.migrate", ~w(-r Air.Repo --quiet))

# delete possible leftovers (e.g. from the integration test)
Air.Repo.delete_all("data_sources_groups")
Air.Repo.delete_all(Air.Schemas.View)
Air.Repo.delete_all(Air.Schemas.AnalystTable)
Air.Repo.delete_all(Air.Schemas.ResultChunk)
Air.Repo.delete_all(Air.Schemas.Query)
Air.Repo.delete_all(Air.Schemas.DataSource)
Air.Repo.delete_all(Air.Schemas.AuditLog)
Air.Repo.delete_all(Air.Schemas.License)

:ok = "priv/dev_license.lic" |> File.read!() |> Air.Service.License.load()

Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, :manual)

# delete all shadow dbs
params = Air.PsqlServer.ShadowDb.connection_params()

{:ok, pid} =
  Postgrex.start_link(
    hostname: params.host,
    database: params.name,
    username: params.user,
    port: params.port,
    ssl: params.ssl,
    password: params.password
  )

result =
  Postgrex.query!(
    pid,
    "SELECT distinct datname FROM pg_catalog.pg_database WHERE datname LIKE 'aircloak_shadow%'",
    []
  )

result.rows
|> List.flatten()
|> Enum.each(&Postgrex.query!(pid, "DROP DATABASE \"#{&1}\"", []))
