test = fn
  :test ->
    {:sequence,
     [
       "MIX_ENV=test ./check_warnings.sh",
       "MIX_ENV=test mix lint",
       "MIX_ENV=test mix test",
       "mix bom --node ../air/assets/node_modules --elixir ../air/deps --elixir ../cloak/deps --rust ../cloak/src/rodbc --validate /tmp"
     ]}

  :dev ->
    {:sequence,
     [
       "MIX_ENV=dev ./check_warnings.sh",
       "mix docs",
       "mix lint",
       "make check-format",
       "MIX_HOME=_build make dialyze"
     ]}
end

# jobs map
%{
  compile:
    {:sequence,
     [
       "make deps",
       "mix compile",
       "MIX_ENV=test mix compile",
       "MIX_HOME=_build mix dialyze --no-analyse"
     ]},
  test:
    {:sequence,
     [
       "make deps",
       {:parallel, [test.(:test), test.(:dev)]}
     ]}
}
