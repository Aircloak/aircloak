ExUnit.start(exclude: [:disabled, :pending], capture_log: true)

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

Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, :manual)
