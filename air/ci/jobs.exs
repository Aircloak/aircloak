%{
  compile: [
    "make deps",
    "MIX_ENV=dev mix compile",
    "MIX_ENV=test mix compile",
    "MIX_ENV=prod mix compile",
    "MIX_HOME=_build mix dialyze --no-analyse --no-compile",
  ],
  test: [
    "make deps",
    "make check-warnings",
    "make docs",
    "make lint",
    "MIX_ENV=test mix recreate_db",
    "make test",
    "MIX_HOME=_build mix dialyze --no-compile"
  ],
}
