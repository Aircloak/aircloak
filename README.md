Air
==========

[![Build
Status](https://magnum.travis-ci.com/Aircloak/web.svg?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/web)

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
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

These features are provided through the following endpoints:

- Web user interface (hello.aircloak.com)
- Internal infrastructure API (infrastructure-api.aircloak.com)
- Analysts API (analyst-api.aircloak.com)


# What is it made up of

The air system consists of following components:

- `etcd` - Dockerized KV registry where system configuration is stored. All other components retrieve their settings from here (e.g. database settings, or addresses of other services)
- `db` - Dockerized database server (used only for development and testing)
- `backend` - Erlang system which implements various processes in the air system, such as background and periodic jobs.
- `frontend` - Web user interface
- `docker_registry` - Containerized Docker registry (needed for CoreOS)
- `coreos` - Vagrant powered CoreOS system that runs `backend` and `frontend` docker containers.

Specific details of each component are describe in `README.md` in their corresponding folders. This document provides general instructions on starting the entire system and deploying.

# Getting started

## Running

There are two ways of running the system:

1. Run most of the components on a localhost.
2. Run each component inside a docker container.

Regardless of the approach you take, etcd, database server, and nginx balancer are always running as docker containers.

The first approach is the one you should usually use for standard development. Here you run almost everything locally (save etcd and db), which makes it simpler to develop, experiment, and test.

In the second approach you run docker containers for each component, which allows you to test the production system on your local machine.

### Prerequisites

In order to run the system you need the following components:

- Docker 1.7 (+ boot2docker if on OS X)
- Ruby 2.0
- Erlang 17.5
- Nginx 1.9 built with `--with-stream` option
- Any other package needed to build and run specific components (e.g. liblua, libprotobuf, ...)

You will also need to add following to your `/etc/hosts`:

```
127.0.0.1 backend.local
127.0.0.1 frontend.local
```

__Linux developers__: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

__OS X Users__: please see [here](osx_setup.md) for additional instructions.

### Starting the required components

First, you need to build the required docker images with following commands:

```
$ db/build-image.sh
$ balancer/build-image.sh
```

Then, you can start all required components with:

```
$ ./start_dependencies.sh
```

__Note__: some sane default settings are provided. If you need to override them, see [here](etcd/README.md#overriding-settings).

You can migrate the database with `cd frontend && bundle exec rake db:migrate`.

If you want to transfer your previous data from the localhost database to the docker one, see [here](db/README.md#migrating-data) for instructions.

### Running the system on the localhost

Make sure that all dependencies have been fetched, that needed components (e.g. backend) have been built, and the required components are started (see above).

Now you can start frontend and backend in the usual way:

```
web/frontend $ bundle exec rails s
web/backend $ make start
```

If all is well, you should be able to access the web via `frontend.local:8201`. If all data is migrated, you should see all clusters/cloaks (make sure to impersonate the analyst), and run tasks in the sandbox.

### Running the system on docker containers

Make sure that the required components are started (see above).

Build images and start containers in the foreground:

```
$ backend/build-image.sh
$ backend/container.sh console

$ frontend/build-image.sh
$ frontend/container.sh console

$ balancer/build-image.sh
$ balancer/container.sh console
```

If everything is fine, you should be able to access the web via `frontend.local:8200`.

### Running the system on CoreOS (experimental)

It is also possible to start the system inside a Vagrant powered CoreOS system. See [here](./coreos/README.md) for details.

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

Nginx sites and rules reside at `/etc/nginx/sites-enabled/*`.
For various configuration settings, see [here](etcd/README.md#production-settings).

### Logs

- Etcd, unicorn stdout, rails, erlang logs: `/var/log/syslog`
- Localhost nginx: `/var/log/nginx/*.log`
- Frontend nginx: `/aircloak/air/frontend/log/nginx-*.log`
- Frontend unicorn stderr: `/aircloak/air/frontend/log/unicorn.stderr.log`

### Typical tasks

- Stop the system: `/etc/init.d/air stop`
- Start the system: `/etc/init.d/air start` (stops it first, if needed)
- Shell to the running container:
    - `/aircloak/air/frontend/container.sh remsh`
    - `/aircloak/air/backend/container.sh remsh`
- Shell to Rails/Erlang console:
    - `/aircloak/air/frontend/container.sh remote_console`
    - `/aircloak/air/backend/container.sh remote_console`
