test = fn
  :test ->
    {:sequence, [
      "MIX_ENV=test ./check_warnings.sh",
      {:parallel, [
        "MIX_ENV=test mix lint",
        "MIX_ENV=test mix test"
      ]}
    ]}

  :dev ->
    {:sequence, [
      "MIX_ENV=dev ./check_warnings.sh",
      {:parallel, [
        "mix docs",
        "mix lint",
        "make check-format",
        "MIX_HOME=_build make dialyze",
      ]}
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
        "MIX_HOME=_build mix dialyze --no-analyse",
      ]}
    ]},

  test:
    {:sequence, [
      "make deps",
      {:parallel, [test.(:test), test.(:dev)]}
    ]},
}
