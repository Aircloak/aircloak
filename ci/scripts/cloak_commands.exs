%{
  compile: [
    "make deps",
    "MIX_ENV=dev mix compile",
    "MIX_ENV=test mix compile",
    "MIX_ENV=prod mix compile",
    "MIX_HOME=_build mix dialyze --no-analyse --no-compile",
  ],

  standard_test: [
    "MIX_ENV=dev mix compile --warnings-as-errors --all-warnings",
    "MIX_ENV=test mix compile --warnings-as-errors --all-warnings",
    "MIX_ENV=prod mix compile --warnings-as-errors --all-warnings",
    "mix lint",
    "MIX_ENV=test mix lint",
    # hacky solution for recreating the test database
    "MIX_ENV=test mix gen.test_data dockerized_ci 1",
    "mix test",
    "MIX_HOME=_build mix dialyze --no-compile"
  ]
}
