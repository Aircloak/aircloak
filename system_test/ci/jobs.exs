%{
  compile: "make",
  system_test: {:sequence, ["make deps", "make test"]}
}
