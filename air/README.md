Air
==========

[![Build
Status](https://magnum.travis-ci.com/Aircloak/web.svg?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/web)

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Getting started](#getting-started)
    - [Running](#running)
    - [Versioning](#versioning)
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

- `site` - The new Air system (insights) which unifies `frontend` and `backend` roles.
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

- Docker 1.8.3 (+ docker-machine if on OS X)
- Ruby 2.0
- Erlang 17.5
- Nginx (preferably 1.9.3) (it needs to be in the execution path for the non-root user)
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

__Note__: nginx might complain that it has no permissions on `/var/log`. You can safely ignore this message, since all logs go to standard output.

You'll need to one-time add some entries to your `/etc/hosts`. Watch the end of the output from the script for information.

__Note__: some sane default settings are provided. If you need to override them, see [here](etcd/README.md#overriding-settings).

You can migrate the database with `cd frontend && bundle exec rake db:migrate`.

If you want to transfer your previous data from the localhost database to the docker one, see [here](db/README.md#migrating-data) for instructions.

### Running the system on the localhost

Make sure that all dependencies have been fetched, that backend is built (`cd backend && make`), and the required components are started (see above).

Now you can start components in the usual way:

```
air/site $ make start
```

If all is well, you should be able to access the web via https://insights.air-local:20000. Note that we use self-signed certificate, so you'll likely get an error in your browser. You need to import the certificate (located in `router/dev_certs/aircloak.com.chain.pem`) to your browser.

If all data is migrated, you should see all clusters/cloaks (make sure to impersonate the analyst), and run tasks in the sandbox.

### Running the system on docker containers

You can start the entire system as docker containers. This gives you an environment very similar to the real production. In particular, each component is running in production mode. Moreover, the balancer container is started, which allows you to test the complete production request path (`balancer -> router -> service`).

To start the system, you can invoke `./dockerized_air.sh start` which will rebuild all images and start required containers in background. If everything is fine, you should be able to access the web via https://frontend.air-local:20100.

If you want to start each container separately in a foreground, make sure that the required components are started with `./start_dependencies.sh`.

Build images and start containers in the foreground:

```
$ site/build-image.sh
$ site/container.sh console

$ router/build-image.sh
$ router/container.sh console

$ balancer/build-image.sh
$ balancer/container.sh console
```

### Running the system on CoreOS

It is also possible to start the system inside a Vagrant powered CoreOS system. See [here](./coreos/README.md) for details.

## Versioning

Production docker images are versioned as `major.minor.patch`. The `major.minor` version is defined in the [version file](./VERSION). If you introduce some breaking changes, just adapt this file accordingly.

The `patch` part is appended automatically when building images during the deploy process according to following rules:

- If `major.minor` has changed, then `patch` will have the value of 0.
- If `major.minor` hasn't changed, but the image contents is different, then the patch version is increased by 1.
- If there's no change in the `major.minor` and the image contents is the same, then there's effectively no change in the image, and we keep the same version.

This means that with time, different images will have different `patch` versions. However, `major.minor` part will always be the same.

This versioning scheme allows us to:

- Restart only changed services during the deploy.
- Restart all containers for breaking changes.
- Keep a couple of previous versions for each image, so we can rollback if needed.
- Remove older images to preserve disk space.

## Deploying

Depending on where you want to deploy (production or stage), you can use following commands:

```
# deploy to production
bundle exec cap air_prod deploy

# deploy to stage
bundle exec cap air_stage deploy
```

This will trigger the rebuild of all images on the build server. Then, the rolling upgrade is performed, updating one machine at the time.

To deploy from a specific branch, you can run `AIR_DEPLOY_BRANCH=another_branch bundle exec cap target deploy`.
If you want to deploy current branch, assuming you're not in the detached head state, you can run:

```
AIR_DEPLOY_BRANCH=$(git symbolic-ref --short HEAD) bundle exec cap target deploy
```

Where `target` is `air_prod` or `air_stage`.

Note that this will work only if the current branch is pushed to the origin.

### Force rebuilding images

Sometimes you may want to rebuild all the images, for example to upgrade Debian packages. To achieve this, open the `common/docker_helper.sh` file, find the function `air_init` and update the `upgrade_date` variable.

In addition, prior to deploy on the production and stage servers you can perform `docker pull` of all base images which are used (e.g. `debian:jessie`). This step is optional, but it's advised you do this when forcing rebuild of all images.

Once the changes are merged, the next deploy will trigger the full rebuild of all images.

## Production

For detailed explanation of the production and staging system see [here](./coreos_cluster.md).

### Services and logs

All services are running as `systemd` units and are logged through `journald`. All logs are forwarded to `aclog1`. To view logs of all Air services, you can ssh to `aclog1` and run following:

```
# tail production logs
tail -f /var/log/syslog | egrep 'accos.. air\-'

# tail staging logs
tail -f /var/log/syslog | egrep 'stage-accos.. air\-'
```

If you want to analyze a single machine in the cluster, you can ssh to that machine and use [journalctl](http://www.freedesktop.org/software/systemd/man/systemctl.html) to see the logs of each units, and [systemctl](http://www.freedesktop.org/software/systemd/man/systemctl.html) to list all units.

Here are some typical commands (you need to run those on a CoreOS machine):

- List all Air units: `systemctl list-units | grep air`
- See the status of a unit: `systemctl status unit_name` (e.g. `air-frontend`)
- Dump unit log: `sudo journalctl -u unit_name --no-pager`
- Tail unit log: `sudo journalctl -u unit_name -f`

You can also use the asterisk wildcard in the `unit_name` (e.g. `sudo journalctl -u air* -f` to tail logs for all Air units).

### Typical tasks

__Note:__ These need to be executed on a CoreOS machine.

- Stop Air units: `sudo /aircloak/air/air_service_ctl.sh stop_system`
- Start the system: `sudo coreos-cloudinit --from-file=/var/lib/coreos-install/user_data`
- Shell to the running container:
    - `/aircloak/air/frontend/container.sh ssh`
    - `/aircloak/air/backend/container.sh ssh`
    - `/aircloak/air/router/container.sh ssh`
- Shell to Rails/Erlang console:
    - `/aircloak/air/frontend/container.sh remote_console`
    - `/aircloak/air/backend/container.sh remote_console`
- Maintenance mode:
    - `/aircloak/air/router/container.sh maintenance_on`
    - `/aircloak/air/router/container.sh maintenance_off`
  __Note__: in a cluster of machines, it's enough to set maintenance on one router only. This will affect the entire cluster.
- To see the status of the etcd cluster you can use [etcdctl](https://github.com/coreos/etcd/tree/master/etcdctl). For example:
    - `sudo etcdctl --endpoint http://127.0.0.1:20120 cluster-health`

## Exposed container ports

Various services listen on different ports. In addition, a port used by each service depends on the environment. We distinguish between three environments:

- dev - Used by components which run locally. Ports are in range 20000-20099.
- prod - Used by docker containers. Ports are in range 20100-20199.
- test - Used in tests. Ports are in range 20200-20299.

A consistent rule is applied for ports in different environments. For example, if a service uses the port 20005 in dev, then it will use ports 20105 and 20205 in other environments.

List of all services and used ports is specified [here](./config/tcp_ports.json).

__OS X developers__: Some ports need to be forwarded to docker-machine in VirtualBox, see [here](osx_setup.md#port-forwarding) for additional instructions.
