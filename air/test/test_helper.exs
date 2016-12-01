ExUnit.start(exclude: [:disabled])

Mix.Task.run "ecto.create", ~w(-r Air.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Air.Repo --quiet)

# delete possible leftovers (e.g. from the integration test)
Air.Repo.delete_all("data_sources_groups")
Air.Repo.delete_all(Air.Schemas.DataSource)
Air.Repo.delete_all(Air.Schemas.AuditLog)

Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, :manual)
