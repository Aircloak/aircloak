ExUnit.start(exclude: [:disabled])

Mix.Task.run "ecto.create", ~w(-r Air.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Air.Repo --quiet)
Ecto.Adapters.SQL.Sandbox.mode(Air.Repo, :manual)
