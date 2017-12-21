%{
  compile: [
    "make deps",
    "MIX_ENV=dev mix compile",
    parallel: [
      "MIX_ENV=test mix compile",
      "MIX_ENV=prod mix compile",
      "MIX_HOME=_build mix dialyze --no-analyse --no-compile"
    ],
  ],
  test: [
    "make deps",
    parallel: [
      "MIX_ENV=dev mix compile --warnings-as-errors --all-warnings",
      "MIX_ENV=test mix compile --warnings-as-errors --all-warnings",
      "MIX_ENV=prod mix compile --warnings-as-errors --all-warnings",
    ],
    parallel: [
      "make docs",
      "make lint",
      "MIX_HOME=_build mix dialyze --no-compile",
      sequence: [
        "MIX_ENV=test mix recreate_db",
        "make test",
      ]
    ]
  ],
}
