#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

# Generate dummy config/routers if needed
mkdir -p config
if [ ! -f config/routers ]; then
  echo "127.0.0.1" > config/routers
fi

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

DOCKER_IMAGE="aircloak/air_balancer"
DOCKER_START_ARGS="--net=host -v $(pwd)/config:/aircloak/balancer/config"
CONTAINER_NAME="air_balancer"
CONTAINER_ARGS="/aircloak/balancer/start.sh"

container_ctl $@