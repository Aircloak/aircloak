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

## Running

To start the system, simply invoke `COREOS_HOST_IP=x.y.z coreos/start.sh`, where `COREOS_HOST_IP` should be set to the IP address of your machine. This address will be used by the VM and Docker containers to access the Docker registry and the database on your machine.

If everything succeeded, you should be able to access the web server on port `8080` on your machine.

If you want to ssh to the machine, you can run `vagrant ssh` from the `coreos` folder.


# What is it made up of

This is a customized version of the [official coreos vagrant box](https://github.com/coreos/coreos-vagrant).
