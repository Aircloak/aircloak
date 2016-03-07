#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh
. ../config/config.sh

DOCKER_IMAGE="registry"
DOCKER_IMAGE_VERSION="2"
DOCKER_START_ARGS="-p $(get_tcp_port prod registry/http):5000"
CONTAINER_NAME="air_docker_registry"

container_ctl $@
