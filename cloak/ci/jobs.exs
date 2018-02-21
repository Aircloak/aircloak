test = fn
  :test ->
    {:sequence, [
      "MIX_ENV=test ./check_warnings.sh",
      {:parallel, ["MIX_ENV=test mix lint",
        {:sequence, [
          "mongod --fork --logpath /var/log/mongodb.log",
          "CLOAK_DATA_SOURCES=postgresql9.4 mix cloak.create_db dockerized_ci",
          "CLOAK_DATA_SOURCES=postgresql9.4 mix test --include exclude_in_dev",
        ]}
      ]}
    ]}

  :dev ->
    {:sequence, [
      "MIX_ENV=dev ./check_warnings.sh",
      {:parallel, [
        "mix docs",
        "mix lint",
        "mix bom --elixir deps /tmp/",
        "MIX_HOME=_build make dialyze",
      ]}
    ]}

  :prod ->
    {:sequence, ["MIX_ENV=prod ./check_warnings.sh"]}
end

# jobs map
%{
  compile:
    {:sequence, [
      "make",
      {:parallel, [
        "MIX_ENV=test make",
        "MIX_ENV=prod make",
        "MIX_HOME=_build mix dialyze --no-analyse",
      ]}
    ]},

  test:
    {:sequence, [
      # Invoking `make` ensures that dependencies are up to date. Normally, this already happens during compilation.
      # However, if in the meantime something new has been committed to the target branch, we need to refetch deps.
      "MIX_ENV=test make",
      {:parallel, [test.(:test), test.(:dev), test.(:prod)]}
    ]},

  compliance:
    {:sequence, [
      # Invoking `make` ensures that dependencies are up to date. Normally, this already happens during compilation.
      # However, if in the meantime something new has been committed to the target branch, we need to refetch deps.
      "MIX_ENV=test make",
      "MIX_ENV=test mix gen.test_data dockerized_ci 100",
      "mix test --only compliance --max-cases 10",
    ]},
}
