#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh

DOCKER_IMAGE="aircloak/performance_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_START_ARGS="-p 15432:5432"
CONTAINER_NAME="performance_db"
CONTAINER_ARGS="postgres -c config_file=/etc/postgresql/postgresql.conf"

container_ctl $@
