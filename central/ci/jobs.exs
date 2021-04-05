test = fn
  :test ->
    {:sequence,
     [
       "MIX_ENV=test ./check_warnings.sh",
       {:parallel, ["MIX_ENV=test mix lint", {:sequence, ["MIX_ENV=test mix central.recreate_db", "mix test"]}]}
     ]}

  :dev ->
    {:sequence,
     [
       "MIX_ENV=dev ./check_warnings.sh",
       {:parallel,
        [
          "make docs",
          "mix lint",
          "make check-format",
          "MIX_HOME=_build make dialyze"
        ]}
     ]}

  :prod ->
    {:sequence, ["MIX_ENV=prod ./check_warnings.sh"]}
end

# jobs map
%{
  compile:
    {:sequence,
     [
       "make deps",
       "mix compile",
       {:parallel,
        [
          {:sequence,
           [
             "MIX_ENV=test mix compile",
             "MIX_ENV=prod mix compile"
           ]},
          "MIX_HOME=_build mix dialyze --no-analyse"
        ]}
     ]},
  test:
    {:sequence,
     [
       "make deps",
       {:parallel, [test.(:test), test.(:dev), test.(:prod)]}
     ]}
}
