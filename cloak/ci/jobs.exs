test = fn
  :test ->
    {:sequence,
     [
       "MIX_ENV=test ./check_warnings.sh",
       "MIX_ENV=test mix lint",
       "CLOAK_DATA_SOURCES=postgresql9.6 mix cloak.create_db dockerized_ci",
       "CLOAK_DATA_SOURCES=postgresql9.6 mix test --include exclude_in_dev"
     ]}

  :dev ->
    {:sequence,
     [
       "MIX_ENV=dev ./check_warnings.sh",
       "mix docs",
       "mix lint",
       "make check-format",
       "mix bom --elixir deps --rust src/rodbc /tmp/",
       "MIX_HOME=_build make dialyze"
     ]}

  :prod ->
    {:sequence, ["MIX_ENV=prod ./check_warnings.sh"]}
end

# jobs map
%{
  compile:
    {:sequence,
     [
       "make",
       "MIX_ENV=test make",
       "MIX_ENV=prod make",
       "MIX_HOME=_build mix dialyze --no-analyse"
     ]},
  test:
    {:sequence,
     [
       # Invoking `make` ensures that dependencies are up to date. Normally, this already happens during compilation.
       # However, if in the meantime something new has been committed to the target branch, we need to refetch deps.
       "MIX_ENV=test make",
       {:parallel, [test.(:test), test.(:dev), test.(:prod)]}
     ]},
  compliance:
    {:sequence,
     [
       # Invoking `make` ensures that dependencies are up to date. Normally, this already happens during compilation.
       # However, if in the meantime something new has been committed to the target branch, we need to refetch deps.
       "MIX_ENV=test make",
       "MIX_ENV=test GLOBAL_DB_NAMESPACE=compliance_test mix gen.test_data dockerized_ci 100",
       "GLOBAL_DB_NAMESPACE=compliance_test mix test --only compliance --max-cases 10"
     ]}
}
