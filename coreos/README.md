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

This component starts the cluster of three CoreOS machines (powered by a Vagrant image). On each machine `frontend`, `backend`, and `router` containers are started. Moreover, CoreOS etcd is used for configuration. All other components (database and docker registry) must be started on your local machine.

Finally, when running locally, a local TCP balancer is started which serves as the single access point.

# Getting started

## Prerequisites

- Vagrant (at least 1.6.3) and VirtualBox
- Nginx 1.9.3 built with `--with-stream` option
- Docker containers for [database](../db/README.md) and [registry](../docker_registry/README.md) must be started.
- Frontend and backend images must be rebuilt after the docker registry is started.

__Network__: CoreOS and Docker containers will communicate with your own machine. For this to work, you need to either disable firewall on your machine or open necessary ports.

__OS X__: Make sure that ports 5000 and 5433 are forwarded to your `boot2docker` VM. The _Host IP_ value in VirtualBox settings should be left empty.

## Running

To start the system, simply invoke `COREOS_HOST_IP=x.y.z coreos/start.sh`, where `COREOS_HOST_IP` should be set to the IP address of your machine. This address will be used by the VM and Docker containers to access the Docker registry and the database on your machine.

If everything succeeded, you should be able to access the web server on https://frontend.air-local:8999.

### Interacting with the machines

If you want to ssh to a machine, you can run `vagrant ssh air-0x` from the `coreos` folder, where `x` is a number between `1` and `cluster_size`.

Service log can be dumped with `journalctl -u service@x --no-pager` where `service` is the service name (e.g. `backend`, `router`), and `x` is an integer representing the instance number. You can fetch logs of the instance running on the machine you're running this command. To see which instance runs on on which machine, just invoke `fleetctl list-units`.

To get shells to the running container, you can invoke:

- `/aircloak/air/frontend/container.sh ssh`
- `/aircloak/air/backend/container.sh ssh`
- `/aircloak/air/router/container.sh ssh`
- `ETCD_PORT=4001 /aircloak/air/frontend/container.sh remote_console`
- `/aircloak/air/backend/container.sh remote_console`


# What is it made up of

This is a customized version of the [official coreos vagrant box](https://github.com/coreos/coreos-vagrant).
