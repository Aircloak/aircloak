# js_sandbox

----------------------

- [What it does](#what-it-does)
- [Typical tasks](#typical-tasks)

----------------------

## What it does

This component is an OTP application which exposes simple interface for executing JavaScript code on the server. Internally, it uses SpiderMonkey, wrapped as an external Erlang port, to run JavaScript code. The port implementation resides in the `c_src` folder.

## Typical tasks

There are following tasks available:

- `make` - compiles the project
- `make test` - runs tests
- `mix coveralls.html` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)
- `make dialyze` - runs the dialyzer
- `make lint` - style checks the Elixir code
- `make docs` - builds the documentation
