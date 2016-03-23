# elixir_common

----------------------

- [What it does](#what-it-does)
- [Typical tasks](#typical-tasks)

----------------------

## What it does

This component is an OTP application which contains the common Elixir code used in other components, such as Air and Cloak. The project includes various common modules, as well as some helper mix tasks.

## Typical tasks

There are following tasks available:

- `make` - compiles the project
- `make test` - runs tests
- `mix test --cover` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)
- `make dialyze` - runs the dialyzer
- `make lint` - style checks the Elixir code
- `make docs` - builds the documentation
