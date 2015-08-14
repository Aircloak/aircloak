Air backend
==========

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Building, running and testing](#building-running-and-testing)
    - [Integration tests](#integration-tests)

----------------------

# What it does

This component exposes internal REST endpoint that is used by the frontend component to perform various long running tasks. In addition, the backend component is responsible for periodic (cron-like) execution of air-related tasks.

# Getting started

## Building, running and testing

You need to have Erlang 17.5 installed and need all dependencies required by `cloak_core`. OS X users need to tweak some compile parameters (see [below](#os-x-specifics)).

Following commands are available:

You can use following commands:

- `make` - fetches dependencies, builds the entire project
- `make app` - builds the main project only
- `make start` - builds the main project only and starts the backend component
- `make test` - runs all tests
- `make dialyzer` - runs the dialyzer

### OS X specifics

If you're running on OS X, you'll need to tweak lua sandbox compile parameters. Create the file `sandbox_makefile.local` directly in the `backend` folder. Paste following contents into the file:

```
CC=gcc
CPP=cpp-4.2
LUALIB=lua.5.1
LUAINCPATH=
```

See [here](https://github.com/aircloak/cloak-core/#building-the-sandbox) for more info.

## Integration tests

The backend provides integration tests that are run against our full infrastructure to validate correct
behaviour. These tests are initiated and run by `integration_tests.erl`.

When running the integration tests locally, make sure the following holds true:

- you have `cloak-core` and `airpub` running and setup correctly
- the frontend is running
- you have exactly one unused local cloak that points to the locally running `cloak-core` instance

### Tests

Currently the only test available is the `cluster_integration_test`. It creates a 3-node cluster
with a build from the `develop` branch, uploads 100GB of data to it, and runs a task.
In local mode the parameters are somewhat different. See `../etcd/etcd_values_dev` for details.

The test is scheduled once a week for Saturday mornings.
To start it manually, call `cluster_integration_test:run()` from the console.
