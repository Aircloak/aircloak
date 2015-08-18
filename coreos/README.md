CoreOS
==========

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Running](#running)
- [What is it made up of](#what-is-it-made-up-of)

----------------------

# What it does

This component runs the Air system in a CoreOS Vagrant image. In particular, `frontend` and `backend` containers run on CoreOS machines. Moreover, CoreOS etcd is used for configuration. All other components (database and docker registry) must be started on your local machine.

# Getting started

## Prerequisites

- Vagrant (at least 1.6.3) and VirtualBox
- Docker containers for [database](../db/README.md) and [registry](../docker_registry/README.md) must be started.
- Frontend and backend images must be rebuilt after the docker registry is started.
- Local frontend and backend containers should not be running.

__Network__: CoreOS and Docker containers will communicate with your own machine. For this to work, you need to either disable firewall on your machine or open necessary ports.

__OS X__: Make sure that ports 5000 and 5433 are forwarded to your `boot2docker` VM. The _Host IP_ value in VirtualBox settings should be left empty.

## Running

To start the system, simply invoke `COREOS_HOST_IP=x.y.z coreos/start.sh`, where `COREOS_HOST_IP` should be set to the IP address of your machine. This address will be used by the VM and Docker containers to access the Docker registry and the database on your machine.

If everything succeeded, you should be able to access the web server on port `8081`, `8082`, ... on your machine. The exact number of ports open depends on the configured cluster size.

### Interacting with the machines

If you want to ssh to a machine, you can run `vagrant ssh air-0x` from the `coreos` folder, where `x` is a number between `1` and `cluster_size`.

To see statuses of each service, you can run `fleetctl list-units`.

Logs of the frontend service are located in `/aircloak/air/frontend/log`. Backend service log can be dumped with `journalctl -u backend --no-pager`.

To get shells to the running container, you can invoke:

- `/aircloak/air/frontend/container.sh remsh`
- `/aircloak/air/backend/container.sh remsh`
- `ETCD_PORT=4001 /aircloak/air/frontend/container.sh remote_console`
- `/aircloak/air/backend/container.sh remote_console`


# What is it made up of

This is a customized version of the [official coreos vagrant box](https://github.com/coreos/coreos-vagrant).
