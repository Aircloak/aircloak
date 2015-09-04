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
    - [Exposed container ports](#exposed-container-ports)

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

- `backend` - Erlang system which implements various processes in the air system, such as publishing of task results (airpub), or background and periodic jobs.
- `balancer` - TCP balancer that forwards requests to multiple routers.
- `coreos` - Vagrant powered CoreOS system that runs cluster of air machines.
- `db` - Dockerized database server (used only for development and testing)
- `docker_registry` - Containerized Docker registry (needed for CoreOS)
- `etcd` - Dockerized KV registry where system configuration is stored. All other components retrieve their settings from here (e.g. database settings, or addresses of other services)
- `frontend` - Web user interface
- `router` - http(s) interface that routes all requests.

Specific details of each component are describe in `README.md` in their corresponding folders. This document provides general instructions on starting the entire system and deploying.

# Getting started

## Running

There are two ways of running the system:

1. Run most of the components on a localhost.
2. Run each component inside a docker container.

Regardless of the approach you take, etcd, database server, and router are always running as docker containers.

The first approach is the one you should usually use for standard development. Here you run almost everything locally (save etcd and db), which makes it simpler to develop, experiment, and test.

In the second approach you run docker containers for each component, which allows you to test the production system on your local machine.

### Prerequisites

In order to run the system you need the following components:

- Docker 1.7 (+ boot2docker if on OS X)
- Ruby 2.0
- Erlang 17.5
- Nginx (preferably 1.9.3)
- [jq](https://stedolan.github.io/jq/)
- Any other package needed to build and run specific components (e.g. liblua, libprotobuf, ...)

__Linux developers__: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

__OS X Users__: please see [here](osx_setup.md) for additional instructions.

### Starting the required components

First, you need to build the required docker images with following commands:

```
$ db/build-image.sh
```

Then, you can start all required components with:

```
$ ./start_dependencies.sh
```

You'll need to one-time add some entries to your `/etc/hosts`. Watch the end of the output from the script for information.

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

If all is well, you should be able to access the web via https://frontend.air-local:20000. Note that we use self-signed certificate, so you'll likely get an error in your browser. You need to import the certificate (located in `router/dev_certs/aircloak.com.chain.pem`) to your browser.

If all data is migrated, you should see all clusters/cloaks (make sure to impersonate the analyst), and run tasks in the sandbox.

### Running the system on docker containers

You can start the entire system as docker containers. This gives you an environment very similar to the real production. In particular, each component is running in production mode. Moreover, the balancer container is started, which allows you to test the complete production request path (`balancer -> router -> service`).

To start the system, you can invoke `./dockerized_air.sh start` which will rebuild all images and start required containers in background. If everything is fine, you should be able to access the web via https://frontend.air-local:20100 (router endpoint) and https://frontend.air-local:20101 (balancer endpoint).

If you want to start each container separately in a foreground, make sure that the required components are started with `./start_dependencies.sh`.

Build images and start containers in the foreground:

```
$ backend/build-image.sh
$ backend/container.sh console

$ frontend/build-image.sh
$ frontend/container.sh console

$ router/build-image.sh
$ router/container.sh console

$ balancer/build-image.sh
$ balancer/container.sh console
```

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

From a standpoint, the architecture of the system is as follows:

<!--- ASCII diagram made with http://asciiflow.com/ -->
```
 http(s) requests  +------------+         +----------------------+
+------------------>TCP balancer|         |   router container   |
                   +-----+------+         |                      |
                         |                |       +-----+        |
                         +------------------------>nginx|        |
                                          |       +^-+-^+        |
                                          |        | | |         |
                                          +----------------------+
                                                   | | |
                                                   | | |
                                                   | | |
                   +-------------------------+     | | |     +-------------------------+
                   | air_frontend container  |     | | |     |  air_backend container  |
                   |                         |     | | |     |                         |
                   |         +-----+         |     | | |     |    +--------------+     |
                   |         |nginx<---------------+ | +---------->erlang release|     |
                   |         +--+--+         |       |       |    +------+-------+     |
                   |            |            |       |       |           |             |
                   |            |            |       |       |           |             |
                   |         +--v--+         |       |       |           |             |
                   |         |rails|         |       |       |           |             |
                   |         +--+--+         |       |       |           |             |
                   |            |            |       |       |           |             |
                   +-------------------------+       |       +-------------------------+
                                |                    |                   |
                                |                    |                   |
                                |          +---------v--------+          |
                                +---------->etcd_air container<----------+
                                           +------------------+

```

In a cluster setting (e.g. CoreOS), the TCP balancer will run outside of the CoreOS machines and balance the traffic between them. Until we implement the full CoreOS support in production, we are running the TCP balancer container on the single production machine.

For various configuration settings, see [here](etcd/README.md#production-settings).

### Logs

Logs of all services can be found at `/var/log/syslog`.

### Typical tasks

- Stop the system: `/etc/init.d/air stop`
- Start the system: `/etc/init.d/air start` (stops it first, if needed)
- Shell to the running container:
    - `/aircloak/air/frontend/container.sh ssh`
    - `/aircloak/air/backend/container.sh ssh`
- Shell to Rails/Erlang console:
    - `/aircloak/air/frontend/container.sh remote_console`
    - `/aircloak/air/backend/container.sh remote_console`

## Exposed container ports

Various services listen on different ports. In addition, a port used by each service depends on the environment. We distinguish between three environments:

- dev - Used by components which run locally. Ports are in range 20000-20099.
- prod - Used by docker containers. Ports are in range 20100-20199.
- test - Used in tests. Ports are in range 20200-20299.

A consistent rule is applied for ports in different environments. For example, if a service uses the port 20005 in dev, then it will use ports 20105 and 20205 in other environments.

List of all services and used ports is specified [here](./config/tcp_ports.json).

__OS X developers__: These ports need to be forwarded to boot2docker in VirtualBox.
