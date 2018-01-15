test = fn
  :test ->
    {:parallel, ["MIX_ENV=test mix lint", {:sequence, ["MIX_ENV=test mix recreate_db", "mix test"]}]}

  :dev ->
    {:parallel, [
      "make docs",
      "make eslint",
      "make flow",
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
}
