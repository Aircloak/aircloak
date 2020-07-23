[
  inputs: ["mix.exs", "{perftest,config,lib,test,priv,ci}/**/*.{ex,exs}"],
  line_length: 120,
  import_deps: [:lens],
  locals_without_parens: [assert_soon: 1, assert_soon: 2, refute_soon: 1, refute_soon: 2]
]
