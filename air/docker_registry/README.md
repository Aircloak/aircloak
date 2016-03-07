Docker registry
==========

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Running](#running)

----------------------

# What it does

This component runs a docker container with the docker registry. This registry is used to publish docker images, and later to fetch them from within CoreOS machines.

# Getting started

## Running

You can start the registry with `docker_registry/container.sh start`. Once the registry is started, rebuild frontend and backend images. Images will be pushed to the registry, and can be used from CoreOS machines.