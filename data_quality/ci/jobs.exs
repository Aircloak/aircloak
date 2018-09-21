test = fn
  :test ->
    {:sequence,
     [
       "MIX_ENV=test ./check_warnings.sh",
       "make test"
     ]}

  :dev ->
    {:sequence,
     [
       "MIX_ENV=dev ./check_warnings.sh",
       {:parallel,
        [
          "make dialyze",
          "make check-format"
        ]}
     ]}
end

# jobs map
%{
  compile:
    {:sequence,
     [
       "make deps",
       "make compile",
       {:parallel,
        [
          "MIX_ENV=test mix compile",
          "MIX_HOME=_build mix dialyze --no-analyse"
        ]}
     ]},
  test:
    {:sequence,
     [
       "make deps",
       {:parallel, [test.(:test), test.(:dev)]}
     ]}
}
