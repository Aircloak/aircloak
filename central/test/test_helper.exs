ExUnit.start()

Mix.Task.run "ecto.create", ~w(-r Central.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Central.Repo --quiet)

Central.Repo.delete_all(Central.Schemas.License)
Central.Repo.delete_all(Central.Schemas.Query)
Central.Repo.delete_all(Central.Schemas.Customer)

Ecto.Adapters.SQL.Sandbox.mode(Central.Repo, :manual)
