#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh
. ../config/config.sh

DOCKER_START_ARGS="-p $(get_tcp_port prod registry/http):5000 registry:2"
container_ctl air_docker_registry $@
