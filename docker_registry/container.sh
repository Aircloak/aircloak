#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

DOCKER_START_ARGS="-p 5000:5000 registry:2"
container_ctl air_docker_registry $@
