%{
  compile:
    {:sequence, ["mix deps.get", "MIX_ENV=test mix compile"]},

  test:
    {:sequence, [
      "MIX_ENV=test mix await_databases",
      {:parallel, [
        ~s/MIX_ENV=test mix cloak.create_db dockerized_integration/,
        ~s/MIX_ENV=test mix do run --no-start -e Air.Repo.configure, ecto.create -r Air.Repo, ecto.migrate -r Air.Repo/,
        ~s/MIX_ENV=test mix do run --no-start -e Central.Repo.configure, ecto.create -r Central.Repo, ecto.migrate -r Central.Repo/,
      ]},
      "mix test"
    ]},
}
