ExUnit.start()

Mix.Task.run "ecto.create", ~w(-r Central.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Central.Repo --quiet)
Ecto.Adapters.SQL.Sandbox.mode(Central.Repo, :manual)
