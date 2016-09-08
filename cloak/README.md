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
        - [Rolling back](#rolling-back)
        - [Interacting with production cloaks](#interacting-with-production-cloaks)

----------------------


## What it does

This component acts as a query layer over sensitive data. It works together with the [air system](../air/) to allow users to issue queries which return aggregated anonymized statistics (properties and counts).


## Getting started

### Prerequisites

You need to have following installed:

- Erlang and Elixir (see [here](../README.md#prerequisites) for details)
- PostgreSQL 9.4
- packages
    - Linux: `liblua5.1-0-dev`, `protobuf-compiler`, `protobuf-c-compiler`, `libprotobuf-c0-dev`, `unixodbc`, `odbc-postgresql`
    - OS X (homebrew): `lua` (5.1), `protobuf`, `protobuf-c`
- Configured ODBC DSN for PostgreSQL - execute the following as root from the `cloak` folder:
  `odbcinst -i -d -l -f priv/odbc/odbcinst.ini && odbcinst -i -s -l -f priv/odbc/odbc.ini`


__OS X related__:

Before installing Erlang, you need to setup ODBC. See [here](./osx_erlang_with_odbc.md) for details.

Prior to first build, you need to copy `lua_sandbox/Makefile.local-example-MacOSX` to `lua_sandbox/Makefile.local`.


### First build

Make sure you install `hex` and `rebar` with `mix local.hex` and `mix local.rebar`. At this point, you can simply run `make` to fetch all dependencies and compile the project.

### Local configuration

You can override some of the configuration settings by creating a `config/dev.local.exs` file. The format is the same as `config/dev.exs`.

### Preparing the database

You need a local PostgreSQL instance listening on port 5432. It is assumed that you have a user called `postgres` that is a superuser. You can create such user with `CREATE USER postgres WITH SUPERUSER`.

You can create the empty database by running `make regenerate_db`.

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
  "air_socket_url": "<URL OF AIR INSTALLATION>/cloak/socket/websocket",
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

__Gotcha__:

The data source requires one of each of the following to be present in the `parameters` section, in order to produce a `global_id`:

- `uid`, `user`, `username`
- `database`
- `hostname`, `server`

The data source setup will fail if this is not the case.
As DSProxy data sources are only configured with a `url` parameter, you will need to add some extra
shim entries to make them work. A DSProxy connection could therefore look like:

```json
"parameters": {
  "# --": "Required parameter for DSProxy",
  "url": "<DSProxy host>",

  "# --": "Dummy data to get a globally unique ID",
  "hostname": "stage",
  "user": "dsproxy",
  "database": "database-name"
}
```

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

#### Running a local docker container

It is possible to run cloak as a local docker container:

1. Make sure all the air dependencies are started with `../air/start_dependencies.sh`.
2. If needed, create the cloak database on docker with `DB_PORT=20002 ./regenerate_db.sh`.
3. Start the `air` container with `../air/site/container.sh console`.
4. Run `./build_image.sh` to create the docker image.
5. Start the container with `./container.sh console`.

You can now interact with the cloak via the dockerized air (https://insights.air-local:20100).

#### Deploying

Typical deploys:

- `./production.sh deploy_targets/master_prod deploy` - deploy production cloak to `srv-76-133`
- `./production.sh deploy_targets/master_stage deploy` - deploy staging cloak to `srv-76-133`
- `./production.sh deploy_targets/test_<dev-name> deploy` - deploy test cloak to `srv-76-135`

This will deploy all __pushed__ changes from your current local branch.
If you want to add a new standard deployment target, please add it to the `deploy_targets` folder of
this repo.

__Note__: You can run multiple cloaks on the same machine. As long as you're not doing any performance/load tests, that should be fine. `srv-76-135` exists for experimental loads that can potentially affect and break other cloaks. If you need a dedicated machine, feel free to take one. It's worth noting that cloaks running on different machines can still affect each other's performance if they use the same database.

If you want to add the additional configuration, you can add a new folder under `/opt/share/cloak_runtime_configs/`. Take a look at the existing ones (e.g. `prod` or `stage`) for examples. __Note__: `/opt/share` is shared between all thors, so you only need to add the configuration on one thor, and then you can reuse it on all others.

As a convenience, there are deploy targets set up for each developer.
These deploy to the experimental thor node `srv-76-135` and do therefore not affect production cloaks.
These cloaks are available for querying through insights-stage.

#### Rolling back

If something is wrong with the deployed version, you can easily rollback to a previously built version. First you need to list all published versions for the given cloak with `./production.sh deploy_targets/some_cloak_config versions`. This will print all published versions, with the top-most one being the latest published version (the one that was started after the latest deploy). To rollback to the older version, you can run `./production.sh deploy_targets/some_cloak_config rollback desired_version`.

Example:

```bash
# list versions
$ ./production.sh deploy_targets/some_cloak_config versions

0.1.9 (2016-08-18T12:23:14.451047582Z)  # latest version
0.1.8 (2016-08-18T06:46:11.273391659Z)
...

# rollback to the older version
$ ./production.sh deploy_targets/test_sasa rollback 0.1.8
```

#### Interacting with production cloaks

Some typical tasks you can run on a thor machine:

- getting a list of running cloaks - `docker ps`
- logs - `docker logs container`
- restarting a cloak - `docker restart container`
- stopping a cloak - `docker stop container && docker rm container`
- remote bash shell - `docker exec -it container /bin/bash`
- remote iex shell - `docker exec -it master_prod bin/cloak remote_console`

##### Connecting local observer to a production cloak

First you need to tunnel the remote EPMD port:

```
# on a local machine
$ ssh -L 4370:localhost:4369 srv-76-133.mpi-sws.org
```

Here, we're using port 4370 on a local machine to avoid collisions with local EPMD.

Now you can get a list of remote nodes locally:

```
# on a local machine
$ ERL_EPMD_PORT=4370 epmd -names

epmd: up and running on port 4369 with data:
name master_stage at port 34000
name master_prod at port 34001
```

Let's say you want to connect to the stage cloak. Close the previous SSH session, and open a new one which tunnel ports 4369 and 34000:

```
# on a local machine
$ ssh -L 4370:localhost:4369 -L 34000:localhost:34000 srv-76-133.mpi-sws.org
```

Now, you can start a local node and connect it to the remote:

```
# on a local machine
$ ERL_EPMD_PORT=4370 iex --name observer@127.0.0.1 --cookie cloak

iex(observer@127.0.0.1)1> Node.connect(:"master_stage@127.0.0.1")
true

iex(observer@127.0.0.1)2> :observer.start
```

Finally, in the Nodes menu select the remote node, and you'll get an observer view of the production cloak.
