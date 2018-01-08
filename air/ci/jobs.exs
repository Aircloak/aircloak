parallel = fn(commands) -> {:parallel, commands} end
sequence = fn(commands) -> {:sequence, commands} end

test = fn
  :test ->
    sequence.([
      "MIX_ENV=test mix compile --warnings-as-errors --all-warnings",
      parallel.(["MIX_ENV=test mix lint", sequence.(["MIX_ENV=test mix recreate_db", "mix test"])])
    ])

  :dev ->
    sequence.([
      "MIX_ENV=dev mix compile --warnings-as-errors --all-warnings",
      parallel.(["make docs", "make eslint", "make flow", "mix lint", "MIX_HOME=_build make dialyze"])
    ])

  :prod ->
    sequence.(["MIX_ENV=prod mix compile --warnings-as-errors --all-warnings"])
end

# jobs map
%{
  compile:
    sequence.(["make deps", "MIX_ENV=dev mix compile",
      parallel.([
        "MIX_ENV=test mix compile",
        "MIX_ENV=prod mix compile",
        "MIX_HOME=_build mix dialyze --no-analyse"
      ])
    ]),

  test:
    sequence.(["make deps", parallel.([test.(:test), test.(:dev), test.(:prod)])]),
}
