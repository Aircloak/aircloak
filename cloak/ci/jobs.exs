%{
  compile: [
    "make deps",
    parallel: [
      "MIX_ENV=dev mix compile",
      "MIX_ENV=test mix compile",
      "MIX_ENV=prod mix compile",
      "MIX_HOME=_build mix dialyze --no-analyse --no-compile",
    ]
  ],

  test: [
    "make deps",
    parallel: [
      "MIX_ENV=dev mix compile --warnings-as-errors --all-warnings",
      "MIX_ENV=test mix compile --warnings-as-errors --all-warnings",
      "MIX_ENV=prod mix compile --warnings-as-errors --all-warnings",
      "mix lint",
      "MIX_ENV=test mix lint",
      "MIX_HOME=_build mix dialyze --no-compile",
      sequence: [
        "mongod --fork --logpath /var/log/mongodb.log",
        # hacky solution for recreating the test database
        "CLOAK_DATA_SOURCES=postgresql9.4 MIX_ENV=test mix gen.test_data dockerized_ci 1",
        "CLOAK_DATA_SOURCES=postgresql9.4 mix test --include exclude_in_dev",
      ]
    ]
  ],

  compliance: [
    "make deps",
    "MIX_ENV=test mix gen.test_data dockerized_ci 100",
    "mix test --only compliance --max-cases 10",
  ]
}
