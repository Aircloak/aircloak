#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

REGISTRY_URL=${REGISTRY_URL:-""}

if [ "$REGISTRY_URL" != "" ]; then
  REGISTRY_URL="$REGISTRY_URL""/"
fi

if [ "$AIR_ROUTERS" != "" ]; then
  DOCKER_START_ARGS="-e AIR_ROUTERS=$AIR_ROUTERS"
fi

DOCKER_START_ARGS="$DOCKER_START_ARGS \
  --net=host \
  "$REGISTRY_URL"aircloak/air_balancer:latest \
  /aircloak/balancer/start.sh"

REMOTE_CONSOLE_COMMAND="/bin/bash"

container_ctl air_balancer $@