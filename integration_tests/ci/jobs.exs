%{
  compile:
    {:sequence,
     [
       "mix deps.get",
       {:parallel, ["MIX_ENV=test mix compile", "./build_assets.sh"]}
     ]},
  test:
    {:sequence,
     [
       "MIX_ENV=test mix await_databases",
       {:parallel,
        [
          ~s/MIX_ENV=test mix format --check-formatted/,
          ~s/MIX_ENV=test mix cloak.create_db dockerized_integration/,
          ~s/MIX_ENV=test mix do run --no-start -e Air.Repo.configure, ecto.create -r Air.Repo, ecto.migrate -r Air.Repo/,
          ~s/MIX_ENV=test mix do run --no-start -e Central.Repo.configure, ecto.create -r Central.Repo, ecto.migrate -r Central.Repo/
        ]},
       "mix test --exclude acceptance"
     ]}
}
