%{
  compile: [
    "make deps",
    "MIX_ENV=dev mix compile",
    "MIX_ENV=test mix compile",
    "MIX_ENV=prod mix compile",
    "MIX_HOME=_build mix dialyze --no-analyse --no-compile",
  ]
}
