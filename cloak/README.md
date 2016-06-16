# Cloak

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [First build](#first-build)
    - [Preparing the database](#preparing-the-database)
    - [Typical tasks](#typical-tasks)
        - [Running partial tests](#running-partial-tests)
        - [Running a local docker container](#running-a-local-docker-container)
        - [Deploying](#deploying)

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

Make sure you install `hex` and `rebar` with `mix local.hex` and `mix local.rebar`. At this point, you can simply run `make` to fetch all dependencies and compile the project.

### Local configuration

You can override some of the configuration settings by creating a `config/dev.local.exs` file. The format is the same as `config/dev.exs`.

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

__Warning:__ Running deploys in parallel may lead to strange results, so coordinate with colleagues before deploying.

Typical deploys:

- `./deploy.sh deploy_targets/master_prod` - deploy production cloak to `srv-76-133`
- `./deploy.sh deploy_targets/master_stage` - deploy staging cloak to `srv-76-133`

This will deploy all __pushed__ changes from your current local branch.

It is also possible to deploy a custom cloak instance to an arbitrary thor machine:

```bash
./deploy.sh image_category target_machine runtime_configuration cloak_name
```

The parameters have following meaning:

- `image_category` - Custom string which is prepended to the image name. You can use your own name, or the name of the feature you're experimenting with. Use only alphanumerics and underscores.
- `target_machine` - Fully qualified name of the thor machine (e.g. `srv-76-133.mpi-sws.org`) which must be accessible via ssh. __Note__: currently docker is installed only on `srv-76-133`. If you want to use another machine, ask our administrator to install docker there.
- `runtime_configuration` - name of the configuration folder in `/opt/share/cloak_runtime_configs/` on the target machine. Currently we support `prod` and `stage`.
- `cloak_name` - The name which will be given to the cloak and the docker container. Use only alphanumerics and underscores.

__Note__: You can only provide one value for each argument. Quoting spaces (e.g. passing `"srv1 srv2 srv3"` as `target_machine`) will not work properly.

Example:

```bash
./deploy.sh sasa srv-76-133.mpi-sws.org stage sasa
```

__Note__: You can run multiple cloaks on the same machine. As long as you're not doing any performance/load tests, that should be fine. Otherwise, reserve a dedicated machine for your experiments.

If you want to add the additional configuration, you can add a new folder under `/opt/share/cloak_runtime_configs/`. Take a look at the existing ones (e.g. `prod` or `stage`) for examples. __Note__: `/opt/share` is shared between all thors, so you only need to add the configuration on one thor, and then you can reuse it on all others.
