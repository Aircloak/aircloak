Air
==========

[![Build
Status](https://magnum.travis-ci.com/Aircloak/web.svg?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/web)

----------------------

- [What it does](#what-it-does)
- [What it is made up of](#what-it-is-made-up-of)
- [Getting started](#getting-started)
    - [Running](#running)
    - [Deploying](#deploying)
    - [Production](#production)
      - [Logs](#logs)
      - [Typical tasks](#typical-tasks)

----------------------

# What it does

This repository contains the Air system, which provides HTTPS endpoints that allows end-users, such as analysts and cluster maintainers, to perform various tasks, such as:

- Manage cloaks and clusters
- Manage users and their permissions
- Write, test, and execute queries
- Generate private keys

These features are provided through following endpoints:

- Web user interface (hello.aircloak.com)
- Internal infrastructure API (infrastructure-api.aircloak.com)
- Analysts API (analyst-api.aircloak.com)


# What is it made up of

The air system consists of following components:

- `etcd` - Dockerized KV registry where system configuration is stored. All other components retrieve their settings from here (e.g. database settings, or addresses of other services)
- `db` - Dockerized database server (used only for development and testing)
- `backend` - Erlang system which implements various processes in the air system, such as background and periodic jobs.
- `frontend` - Web user interface

Specific details of each component are describe in `README.md` in their corresponding folders. This document provides general instructions on starting the entire system and deploying.

# Getting started

## Running

There are two ways of running the system:

1. Run most of the components on a localhost.
2. Run each component inside a docker container.

Regardless of the approach you take, etcd and database server are always running as docker containers.

The first approach is the one you should usually use for standard development. Here you run almost everything locally (save etcd and db), which makes it simpler to develop, experiment, and test.

In the second approach you run docker containers for each component, which allows you to test the production system on your local machine.

### Prerequisites

In order to run the system you need the following components:

- Docker 1.6 (+ boot2docker if on OS X)
- Ruby 2.0
- Erlang 17.5
- Any other package needed to build and run specific components (e.g. liblua, libprotobuf, ...)

__Linux developers__: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

__OS X Users__: please see [here](osx_setup.md) for additional instructions.


### Starting etcd and database

These two containers are always needed for the rest of the system.

First, you need to invoke one-time building of the database image:

```
$ db/build-image.sh
```

To start the containers, simply run:

```
$ etcd/container.sh start
$ db/container.sh start
```

You can migrate the database with `cd frontend && bundle exec rake db:migrate`.

If you want to transfer your previous data from the localhost database to the docker one, see [here](database_migration.md) for instructions.

### Running the system on the localhost

Default development settings are stored in `etcd/etcd_values_dev` file. If you need to
override some of those settings (for example analyst key password), create the new file
`etcd/local_settings/dev`, then copy and modify needed settings from `etcd/etcd_values_dev`.
You can also add files `test`, `docker`, and `prod` to `etcd/local_settings` folder to
override settings for other environments.

Start `etcd` and `db` containers:

```
$ etcd/container.sh start
$ db/container.sh start
```

Make sure that all dependencies have been fetched, and that needed components (e.g. backend) have been built.

Now you can start frontend and backend in the usual way:

```
web/frontend $ bundle exec rails s
web/backend $ make start
```

If all is well, you should be able to access the web via `localhost:3000`. If all data is migrated, you should see all clusters/cloaks (make sure to impersonate the analyst), and run tasks in the sandbox.

### Running the system on docker containers

Start `etcd` and `db` containers:

```
$ etcd/container.sh start
$ db/container.sh start
```

Make sure that docker specific settings are configured:

```
$ etcd/config_docker.sh
```

__Hint:__ you can revert back to localhost settings by calling `etcd/config_local.sh`.

Build images and start containers in the foreground:

```
$ backend/build-image.sh
$ backend/container.sh console

$ frontend/build-image.sh
$ frontend/container.sh console
```

If everything is fine, you should be able to access the web via `localhost:8080`.

## Deploying

Simply run `bundle exec cap production deploy`, which should deploy the entire air system and migrate the database.

To deploy from a specific branch, you can run `AIR_DEPLOY_BRANCH=another_branch bundle exec cap production deploy`.
If you want to deploy current branch, assuming you're not in the detached head state, you can run:

```
AIR_DEPLOY_BRANCH=$(git symbolic-ref --short HEAD) bundle exec cap production deploy
```

Note that this will work only if the current branch is pushed to the origin.

## Production

On the target server, the architecture of the system is as follows:

<!--- ASCII diagram made with http://asciiflow.com/ -->
```
       +-----------+
       |local nginx|
       +-----+-----+
             |
             |
             |
+------------v------------+
| air_frontend container  |
|                         |
|         +-----+         |
|         |nginx|         |
|         +--+--+         |
|            |            |
|            |            |
|         +--v--+         |
|         |rails+----------------------------+
|         +--+--+         |                  |
|            |            |                  |
+-------------------------+                  |
             |                     +---------v--------+
             |                     |etcd_air container|
             |                     +---------^--------+
+------------v------------+                  |
|  air_backend container  |                  |
|                         |                  |
|    +--------------+     |                  |
|    |erlang release+------------------------+
|    +--------------+     |
|                         |
+-------------------------+

```

Nginx sites and rules are reside at `/etc/nginx/sites-enabled/*`.
For various configuration settings, see [here](etcd/README.md#production-settings).

### Logs

- Etcd, unicorn stdout, rails, erlang logs: `/var/log/syslog`
- Localhost nginx: `/var/log/nginx/*.log`
- Frontend nginx: `/aircloak/air/frontend/log/nginx-*.log`
- Frontend unicorn stderr: `/aircloak/air/frontend/log/unicorn.stderr.log`

### Typical tasks

- Stop the system: `/etc/init.d/air stop`
- Start the system: `/etc/init.d/air start` (stops it if needed)
- Shell to the running container:
    - `/aircloak/air/frontend/container.sh remsh`
    - `/aircloak/air/backend/container.sh remsh`
