# Cloak

- [Cloak](#cloak)
  - [What it does](#what-it-does)
  - [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [First build](#first-build)
    - [Local configuration](#local-configuration)
    - [Preparing the database](#preparing-the-database)
    - [Cloak configuration](#cloak-configuration)
      - [Global identifier](#global-identifier)
      - [Tables](#tables)
    - [Typical tasks](#typical-tasks)
      - [Running partial tests](#running-partial-tests)
      - [Running a specific compliance test](#running-a-specific-compliance-test)
      - [Running a local docker container](#running-a-local-docker-container)
      - [Running full compliance CI locally](#running-full-compliance-ci-locally)
      - [Running fuzz tests](#running-fuzz-tests)
      - [Deploying](#deploying)
      - [Running performance tests locally](#running-performance-tests-locally)
    - [Installing database servers](#installing-database-servers)
      - [SQL Server](#sql-server)
      - [Working in a dev container](#working-in-a-dev-container)

----------------------


## What it does

This component acts as a query layer over sensitive data. It works together with the [air system](../air/) to allow
users to issue queries which return aggregated anonymized statistics (properties and counts).


## Getting started

### Prerequisites

You need to have following installed:

- Erlang and Elixir (see [here](../README.md#prerequisites) for details)
- PostgreSQL 9.4 or more recent
- linux:
  - packages: `unixodbc`, `odbc-postgresql`
  - Configured ODBC DSN for PostgreSQL - execute the following as root from the `cloak` folder:
    `odbcinst -i -s -l -f priv/odbc/odbc.ini`
- macos
  - packages (homebrew): `unixodbc`, `psqlodbc`
  - Configured ODBC DSN for PostgreSQL - execute the following as root from the `cloak` folder:
    `odbcinst -i -s -l -f priv/odbc/osx/odbc.ini
  - Double check the [macos guide](osx_erlang_with_odbc.md) to ensure you have Erlang built
    with support for odbc.
  - `brew install wget` as the Makefile relies on `wget` to install ODBC drivers


### First build

Run `mix deps.get` to install the dependencies, followed by `make` or `make start` to build or build and start
the cloak, respectively.


### Local configuration

You can override some of the configuration settings by creating a `config/dev.local.exs` file. The format is the same as
`config/dev.exs`.

### Preparing the database

You can create a database with initial data by running `make recreate-db`.
It assumes you have a postgres server running on port 20002, which is started by default with [start_dependencies.sh](../start_dependencies.sh).

If everything is properly installed and setup, standard tests invoked with `make test` should pass.

### Cloak configuration

Cloaks have two sets of configuration files.
One set contains static configuration that remain the same across
installations. These configuration files can be found under `config/*`.
The other contain configuration parameters that are potentially unique to a particular installation,
and which are what you are going to adapt when installing a cloak. These configuration files are written
as a JSON file that is mounted into the deployed cloak container.
A sample cloak configuration can be found under `priv/config/dev.json`.

The skeleton of a configuration looks like this:

```json
{
  "air_site": "<URL OF AIR INSTALLATION>",
  "data_sources": []
}
```

The data sources section contain a list of individual data source configurations. A data source
configuration might look like this:

```json
{
  "driver": "<driver-name>",
  "marker": "<optional marker>",
  "parameters": {
    ...
  },
  "tables": {
    "<table name as seen by user>": {
      "db_name": "<table as named in the database>",
      "user_id": "<user id column name in the table>"
    },
    ...
  }
},
```

A data source requires a driver supported by the cloak. For example `postgressql` or `odbc`.
The ODBC connector is a catch-all for drivers that don't have native support in the cloak.

The parameters are used by the driver to connect to the data store. In the case of the `ODBC` driver, the
parameters are concatenated into an ODBC connection string.

#### Global identifier

Data sources are identified by a global identifier created from a combination of the connection parameters.
As a result, multiple distinct cloaks connecting to the same data store, will all make available a data source
with the same id. If you want to make multiple data sources unique despite being backed by the same database and user,
you can use the `marker` option to add more context to the global identifier produced.

#### Tables

The `tables` section lists the set of tables that should be made available to the analyst.
It allows the system to be configured such that the table name presented to the analyst doesn't necessarily
reflect the name used by the underlying data store.


### Typical tasks

- `make start` - starts the system
- `make test` - runs standard tests (unit and simple property tests)
- `make dialyze` - runs the dialyzer
- `make lint` - style checking of Elixir code
- `make doc` - generates the documentation
- `make release-local` - creates the OTP release that can be run locally (on a dev machine)
- `mix coveralls.html` - runs ExUnit tests with test coverage (generates an HTML output in the `cover` folder)


#### Running partial tests

Occasionally you may want to run only some tests. Depending on the kind of test, you have the following commands at your disposal:

- `mix test` - ExUnit tests
- `mix eunit` - EUnit (Erlang) tests
- `make proper` - runs all property tests
- `make proper-std` - runs only simple property tests
- `make proper-extended` - runs extended (longer running) property tests
- `mix test/some_test_script.exs` to run a single ExUnit test suite. See
  [here](http://elixir-lang.org/docs/stable/mix/Mix.Tasks.Test.html) for more information.
- `mix eunit --module erlang_module` EUnit test of a single Erlang module.
- `mix proper --module erlang_module` PropEr test of a single module.

Note that when specifying Erlang modules, you need to provide the name of the real module and not the test one. For
example, let's say you have the module `anonymizer` and property tests are residing in the `anonymizer_test` module. The
corresponding command is `mix proper --module anonymizer` (without the `_test` suffix).

By default, only native PostgreSQL adapter is tested locally, while other drivers are excluded. To change
this you can run following commands:

- `mix test --only compliance` - to run only the compliance tests
- `make test_all` - to run all tests which are running on CI: standard tests, and tests for all other
  database adapters (Oracle, ...). Note however that compliance tests are going to be executed
  on a reduced database set (as specified in `compliance.json`).

In order to have working tests on other drivers, you need to start corresponding database servers locally - see
[Installing database servers](#installing-database-servers).

#### Running a specific compliance test

Each compliance test gets its tag which is reported for each failing test. This simplifies running a single compliance
test. Example:

```
mix test --only "compliance:upper(<col>) changes.change notes_changes"
```

Note that there is no space after `:`.

In addition, tests for each functions are tagged, so one can exercise a single function with:

```
mix test --only "pow(<col1>, <col2>)"
```

#### Running a local docker container

It is possible to run cloak as a local docker container:

1. Make sure all the air dependencies are started with `../air/start_dependencies.sh`.
2. If needed, create the cloak database on docker with `DB_PORT=20002 ./regenerate-db.sh`.
3. Start the `air` container with `../air/container.sh console`.
4. Run `./build_image.sh` to create the docker image.
5. Start the container with `./container.sh console`.

You can now interact with the cloak via the dockerized air (http://localhost:8080 or https://insights.air-local:8443).

#### Running full compliance CI locally

To run the full compliance CI test locally with the command `make ci.compliance`. This will start the required database
containers, start the cloak container, generate test data, and then enter the bash shell in the container. Now, you can
run tests with `mix test --only compliance`. The container uses the source files mounted from your host, so you can
easily edit those files and repeatedly run the tests without needing to rebuild the image.

If you want to test some specific databases, you can set the `CLOAK_DATA_SOURCES` env variable. For example, to test
only PostgreSQL and Oracle, you can run the following command:

```
CLOAK_DATA_SOURCES="postgresql oracle" make ci.compliance
```

The `CLOAK_DATA_SOURCES` env var is a whitespace separated list of data source names which you want to use in the test
suite. For the list of available names, take a look at configuration files in [this folder](priv/config/dockerized_ci).

The default number of generated users is 10. You can change this by setting the `COMPLIANCE_USERS` env variable:

```
COMPLIANCE_USERS=50 make ci.compliance
```

#### Running fuzz tests

A task is available to run randomly-generated queries against the current version of cloak (fuzz testing). To invoke it:

```
MIX_ENV=test mix fuzzer.run --queries 10
```

Note that the `queries` option specifies how many queries to run. This test runs against the same configuration as the
compliance tests.  You can use `mix gen.test_data` to create these databases:

```
MIX_ENV=test mix gen.test_data compliance 10
```

Or you can run from [inside the compliance container](#running-full-compliance-ci-locally) to have access to different
data source types.

The fuzzer creates three output files (by default `/tmp/all.txt`, `/tmp/stats.txt`, and `/tmp/crashes.txt`) where the
results are stored. The `crashes` file contains all queries that failed in an unexpected way along with the details of
the error - this should be the most interesting output for regular usage, the other outputs are mostly meant for
debugging the fuzzer itself. Each query in `crashes` is a potential bug; however, it might also be a bug in the fuzzer.
The `all` file lists all queries executed and the class of result obtained for each of them - `ok` means the query
executed successfully, `unexpected_error` means the query failed in a way unknown to the fuzzer, and the other
categories are different kinds of common errors, like anonymity constraint violations. The `stats` file lists the kinds
of results from the `all` file along with the number of times each result was obtained.

#### Deploying

See [here](../README.md#deploying).

#### Running performance tests locally

Before running the tests you need to prepare the performance database.

- `make recreate-perf-db` - generate a `preformance` DB with 10k users
- `make perftest` - run the performance tests

Note that the tests submit results to InfluxDB - it will be started with `start_dependencies.sh`.

### Installing database servers

#### SQL Server

- Change the memory allowed to docker to at least 3,5 GB
- `make sql-server-container` - starts the container
- `DB_NAME=cloaktest2 make sql-server-database` - creates a database named `cloaktest2`
- Note that connecting to SQL Server will only work in the CI container (`make ci.compliance`)
- The following example section will allow you to add an SQL Server datasource to the appropriate config.json:

```json
{
  "driver": "sqlserver",
  "name": "sql_server",
  "parameters": {
    "hostname": "docker.for.mac.localhost",
    "username": "sa",
    "password": "Sql{}server1",
    "database": "cloaktest2",
    "odbc_parameters": {
      "Port": "1433"
    }
  },
  "tables": {
  }
}
```

#### Working in a dev container

Dev container is built to allow developers who work on macOS to develop against other databases, such as Oracle. Before starting the container, you need to first build the air container and start it locally (with `air/build-image.sh` and `air/container.sh console`). Then, you can start the cloak dev container by going to the cloak folder and invoking `make dev-container`.

Once the container is started, you can start the cloak with `make start`, and visit `http://localhost:8080`. At this point, you should be able to access Oracle datasources.

The source files of the cloak project are mounted into the container. Therefore, you can freely edit the source files, and restart the cloak without needing to rebuild and restart the container.
