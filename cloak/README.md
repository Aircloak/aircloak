# Cloak

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [First build](#first-build)
    - [Preparing the database](#preparing-the-database)
    - [Cloak configuration](#cloak-configuration)
    - [Typical tasks](#typical-tasks)
        - [Running partial tests](#running-partial-tests)
        - [Running a local docker container](#running-a-local-docker-container)
        - [Deploying](#deploying)
    - [Installing database servers](#installing-database-servers)

----------------------


## What it does

This component acts as a query layer over sensitive data. It works together with the [air system](../air/) to allow users to issue queries which return aggregated anonymized statistics (properties and counts).


## Getting started

### Prerequisites

You need to have following installed:

- Erlang and Elixir (see [here](../README.md#prerequisites) for details)
- PostgreSQL 9.4 or more recent
- linux:
  - packages: `unixodbc`, `odbc-postgresql`
  - Configured ODBC DSN for PostgreSQL - execute the following as root from the `cloak` folder:
    `odbcinst -i -d -l -f priv/odbc/odbcinst.ini && odbcinst -i -s -l -f priv/odbc/odbc.ini`
- macos
  - packages (homebrew): `unixodbc`, `psqlodbc`
  - Configured ODBC DSN for PostgreSQL - execute the following as root from the `cloak` folder:
    `odbcinst -i -d -l -f priv/odbc/osx/odbcinst.ini && odbcinst -i -s -l -f priv/odbc/osx/odbc.ini
  - Double check the [macos guide](osx_erlang_with_odbc.md) to ensure you have Erlang built
    with support for odbc.


### First build

Run `mix deps.get` to install the dependencies, followed by `make` or `make start` to build or build and start
the cloak, respectively.


### Local configuration

You can override some of the configuration settings by creating a `config/dev.local.exs` file. The format is the same as `config/dev.exs`.

### Preparing the database

You need a local PostgreSQL instance listening on port 5432. It is assumed that you have a user called `postgres` that is a superuser. You can create such user with `CREATE USER postgres WITH SUPERUSER`.

You can create the empty database by running `make regenerate-db`.

If you want to run cloak in a local docker container, you need to run this command once more to create the database in the PostgreSQL container: `DB_PORT=20002 ./regenerate-db.sh`.

If everything is properly installed and setup, standard tests invoked with `make test` should pass.

#### Working with SAP HANA data sources on local machine

In order to work with SAP HANA data source, you need to choose a schema you'll work on. You can define your schema in the `dev.local.exs` file:

```elixir
config :cloak, :sap_hana, default_schema: your_schema_name
```

Make sure to choose something unique for the schema name, such as your own name. Once you configured the schema, you need to run `make regenerate-db` again.

If you want to run SAP HANA tests locally, you'll also need to add a `test.local.exs` file with the same configuration. You can safely use the same schema in the test environment.

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
      "user_id": "<user id column name in the table>",
      "ignore_unsupported_types": true | false
    },
    ...
  }
},
```

A data source requires a driver supported by the cloak. For example `postgressql`, `mysql` or `odbc`.
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

If `ignore_unsupported_types` is set to `true` the cloak will fail to start if there are columns with data
types not natively supported by the cloak. If it is set to false, these columns will be ignored, and not made
available for analysis to the analyst.


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
- `mix test/some_test_script.exs` to run a single ExUnit test suite. See [here](http://elixir-lang.org/docs/stable/mix/Mix.Tasks.Test.html) for more information.
- `mix eunit --module erlang_module` EUnit test of a single Erlang module.
- `mix proper --module erlang_module` PropEr test of a single module.

Note that when specifying Erlang modules, you need to provide the name of the real module and not the test one. For example, let's say you have the module `anonymizer` and property tests are residing in the `anonymizer_test` module. The corresponding command is `mix proper --module anonymizer` (without the `_test` suffix).

By default, only native PostgreSQL adapter is tested locally, while MongoDB and other drivers are excluded. To change this you can run following commands:

- `mix test --only mongodb` - to run only MongoDB tests
- `mix test --only saphana` - to run only SAP HANA tests
- `mix test --only compliance` - to run only the compliance tests
- `make test_all` - to run all tests which are running on Travis: standard tests, MongoDB tests, and tests for all other database adapters (MySQL, PostgreSQL through ODBC, ...)

In order to have working tests on other drivers, you need to start corresponding database servers locally - see [Installing database servers](#installing-database-servers).

Note that SAP HANA tests can't be executed directly on macOS machines. Instead, you need to start a local development container with `make dev-container`.

#### Running a local docker container

It is possible to run cloak as a local docker container:

1. Make sure all the air dependencies are started with `../air/start_dependencies.sh`.
2. If needed, create the cloak database on docker with `DB_PORT=20002 ./regenerate-db.sh`.
3. Start the `air` container with `../air/container.sh console`.
4. Run `./build_image.sh` to create the docker image.
5. Start the container with `./container.sh console`.

You can now interact with the cloak via the dockerized air (http://localhost:8080 or https://insights.air-local:8443).

#### Running a local development container (macos only)

With the `make dev-container` command, you can start a Linux container with your local aircloak folder mounted. This feature is intended for macos developers, and it allows them to develop and test in the Linux environment. You need to have Docker for Mac 17.0.6 or higher.

#### Deploying

See [here](../README.md#deploying).

#### Running performance tests locally

Before running the tests you need to prepare the performance database.

- `make recreate-perf-db` - generate a `preformance` DB with 10k users
- `make perftest` - run the performance tests

Note that the tests submit results to InfluxDB - it will be started with `start_dependencies.sh`.

### Installing database servers

#### Mongodb

- `make mongo-server-container` - starts the container
- Add something like the following section to the appropriate config.json:

```json
{
  "driver": "mongodb",
  "name": "mongodb",
  "parameters": {
    "hostname": "localhost",
    "username": "root",
    "database": "cloaktest2"
  },
  "tables": {
  }
}
```

#### MySQL

- `make mysql-server-container` - starts the container
- `DB_NAME=cloaktest2 make mysql-server-database` - creates a database named `cloaktest2`
- Add something like the following section to the appropriate config.json:

```json
{
  "driver": "mysql",
  "marker": "connector",
  "name": "mysql",
  "parameters": {
    "hostname": "localhost",
    "username": "root",
    "database": "cloaktest2"
  },
  "tables": {
  }
}
```

#### SQL Server

- Change the memory allowed to docker to at least 3,5 GB
- `make sql-server-container` - starts the container
- `DB_NAME=cloaktest2 make sql-server-database` - creates a database named `cloaktest2`
- Note that connecting to SQL Server will only work in the dev-container (`make dev-container`)
- The following example section will allow you to add an SQL Server datasource to the appropriate config.json:

```json
{
  "driver": "sqlserver",
  "name": "sql_server",
  "parameters": {
    "hostname": "docker.for.mac.localhost",
    "username": "sa",
    "password": "7fNBjlaeoRwz*zH9",
    "database": "cloaktest2",
    "encoding": "utf8",
    "odbc_parameters": {
      "Port": "1433"
    }
  },
  "tables": {
  }
}
```
