#!/bin/bash

# This script is executed on a daily basis, as configured in `aircloak_ci_restart_docker.timer`.
# This is a hacky remedy for occasional docker hangs which we've noticed on the CI server.

# stop services
systemctl stop aircloak_ci
systemctl stop docker

# brutally kill any lingering docker processes
kill -9 `ps aux | grep docker | awk '{print $1}'`

# remove any leftover files
rm -rf /var/run/docker

# start docker
systemctl start docker

# remove any dangling containers
docker rm --force `docker ps -aq`

# start the CI service
systemctl start aircloak_ci
