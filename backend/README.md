air
==========

| Branch      | Build status |
|-------------|--------------|
| develop     | [![Build Status](https://magnum.travis-ci.com/Aircloak/air.png?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/air) |

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Sandbox API](#sandbox-api)
    - [Building, running and testing](#building-running-and-testing)

----------------------

# What it does

This component implements various services needed in the Air system:

- An HTTP interface for running tasks, allowing clients to supply task data and the sandbox code, and in
this way test whether their tasks are working properly.
- A periodical job that deletes old tasks.

# Getting started

## Sandbox API

This is an HTTP server that can be used to perform test runs of the sandbox (Lua) code. By default, it listens
on port 11000. An example request looks like:

```
cat << EOF | \
  curl http://localhost:11000/task/run -v \
       --data @-
    {
      "users_data": {
        "heights": {
          "columns": ["height"],
          "data": {"user1": [[175]]}
        }
      },
      "code": "report_property('height', tables.heights[1].height);",
      // optional
      "libraries": [
        {"name": "lib1", code: "function foo() return 1 end"}
      ]
    }
EOF
```

## Building, running, and testing

Make sure you're using Erlang `17.x`.

### Setting up the database

The database server has to be reachable via `air_db`. There has to be an `air` database login available. You
need to create two databases:

- `aircloakdatabase` - generated through migrations of the `web` component
- `air_test_database` - generated once, with `./recreate_test_db.sh`

### Migrating the database

If the `web` database has been migrated, this needs to be reflected in this project, so that tests can run
normally:

1. Make sure your local `aircloakdatabase` is migrated.
2. Run `./dump_db.sh`
3. Run `./recreate_test_db.sh`
4. Commit and push the new version of `test/db_structure.sql`

### Sandbox compile parameters

If you're running on OS X, you'll need to tweak lua sandbox compile parameters. Create the file `sandbox_makefile.local` directly in the top level folder where the repository is cloned. Paste following contents into the file:

```
CC=gcc
CPP=cpp-4.2
LUALIB=lua.5.1
LUAINCPATH=
```

See [here](https://github.com/aircloak/cloak-core/#building-the-sandbox) for more info.

You can use following commands:

- `make` - fetches dependencies, build the entire project
- `make app` -  fetches dependencies, build the entire project
- `make start` - starts the local HTTP server
- `make test` - runs all tests
- `make dialyzer` - runs the dialyzer

## Running the docker container

To start `air` component as a docker container, you first need to build the release image with `./build.sh`. This will produce an image which you can then run with `./run.sh`.

### OS X specifics

If you're working from a folder which is not situated under your home folder, you need to map it in `boot2docker`. For example, let's say that you're working from the folder `/projects/aircloak/air`. The procedure is following:

1. Add `projects` as the shared folder in VirtualBox under some name (e.g. CODE)
2. Make sure boot2docker VM is started. Run `boot2docker ssh` and create the file `/var/lib/boot2docker/bootlocal.sh` with the following contents:
```
sudo mkdir -p /projects
sudo mount -t vboxsf -o uid=1000,gid=50 CODE /projects
```
3. `chmod +x /var/lib/boot2docker/bootlocal.sh`
4. Exit the machine and do `boot2docker restart`

After the VM is restarted, the `/projects` folder inside the VM should correspond to your own `/projects` folder.

**Note**: this is not needed if the folder is situated under the home folder
