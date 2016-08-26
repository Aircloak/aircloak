ExUnit.start(capture_log: true)

Mix.Task.run "ecto.create", ~w(-r Air.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Air.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(Air.Repo)
