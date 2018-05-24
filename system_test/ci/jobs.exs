%{
  compile: "make",
  test: {:sequence, ["make deps", "make test"]}
}
