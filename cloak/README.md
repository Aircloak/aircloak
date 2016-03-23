# Cloak

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [First build](#first-build)
    - [Preparing the database](#preparing-the-database)
    - [Typical tasks](#typical-tasks)
        - [Running partial tests](#running-partial-tests)

----------------------


## What it does

This component acts as a query layer over sensitive data. It works together with the [air system](../air/) to allow users to issue queries which return aggregated anonymized statistics (properties and counts).


## Getting started

### Prerequisites

You need to have following installed:

- Erlang 18.x
- Elixir >= 1.2.3
- PostgreSQL 9.4
- packages
    - Linux: `liblua5.1-0-dev`, `protobuf-compiler`, `protobuf-c-compiler`, `libprotobuf-c0-dev`
    - OS X (homebrew): `lua` (5.1), `protobuf`, `protobuf-c`


__OS X related__:

Prior to first build, you need to copy `lua_sandbox/Makefile.local-example-MacOSX` to `lua_sandbox/Makefile.local`.


### First build

Make sure you install hex client and rebar with `mix local.hex` and `mix local.rebar`. At this point, you can simply run `make` to fetch all dependencies and compile the project.


### Preparing the database

You need a local PostgreSQL instance listening on port 5432. It is assumed that you have a user called `postgres` that is a superuser. You can create such user with `CREATE USER postgres WITH SUPERUSER`.

You can create the empty database by running `make regenerate_db`.

If everything is properly installed and setup, standard tests invoked with `make test` should pass.


### Typical tasks

- `make start` - starts the system
- `make test` - runs standard tests (unit and simple property tests)
- `make dialyze` - runs the dialyzer
- `make lint` - style checking of Elixir code
- `make doc` - generates the documentation
- `make release` - creates the OTP release


#### Running partial tests

Occasionally you may want to run only some tests. Depending on the kind of test, you have the following commands at your disposal:

- `mix test` - ExUnit tests
- `mix eunit` - EUnit (Erlang) tests
- `make proper` - runs all property tests
- `make proper-std` - runs only simple property tests
- `make proper-extended` - runs extended (longer running) property tests
- `mix test/some_test_script.exs` to run a single ExUnit test suite. See [here](http://elixir-lang.org/docs/stable/mix/Mix.Tasks.Test.html) for more information.
- `mix eunit --module erlang_module` EUnit test of a single Erlang module.
- `mix proper --module erlang_module` PropEr test of a single module.

Note that when specifying Erlang modules, you need to provide the name of the real module and not the test one. For example, let's say you have the module `anonymizer` and property tests are residing in the `anonymizer_test` module. The corresponding command is `mix proper --module anonymizer` (without the `_test` suffix).
