test = fn
  :test ->
    {:parallel, ["MIX_ENV=test mix lint",
      {:sequence, [
        "mongod --fork --logpath /var/log/mongodb.log",
        "CLOAK_DATA_SOURCES=postgresql9.4 mix cloak.create_db dockerized_ci",
        "CLOAK_DATA_SOURCES=postgresql9.4 mix test --include exclude_in_dev",
      ]}
    ]}

  :dev ->
    {:parallel, [
      "mix docs",
      "mix lint",
      "MIX_HOME=_build make dialyze",
    ]}
end

# jobs map
%{
  compile:
    {:sequence, [
      "make deps",
      "mix compile",
      {:parallel, [
        "MIX_ENV=test mix compile",
        "MIX_ENV=prod mix compile",
        "MIX_HOME=_build mix dialyze --no-analyse",
      ]}
    ]},

  test:
    {:sequence, [
      "make deps",
      {:parallel, [test.(:test), test.(:dev)]}
    ]},

  compliance:
    {:sequence, [
      "make deps",
      "MIX_ENV=test mix gen.test_data dockerized_ci 100",
      "mix test --only compliance --max-cases 10",
    ]},
}
