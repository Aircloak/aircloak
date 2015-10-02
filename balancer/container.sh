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

REGISTRY_URL=${REGISTRY_URL:-""}

if [ "$REGISTRY_URL" != "" ]; then
  REGISTRY_URL="$REGISTRY_URL""/"
fi

if [ "$AIR_ROUTERS" != "" ]; then
  DOCKER_START_ARGS="-e AIR_ROUTERS=$AIR_ROUTERS"
fi

DOCKER_START_ARGS="$DOCKER_START_ARGS \
  --net=host \
  -v $(pwd)/config:/aircloak/balancer/config \
  "$REGISTRY_URL"aircloak/air_balancer:latest \
  /aircloak/balancer/start.sh"

REMOTE_CONSOLE_COMMAND="/bin/bash"

container_ctl air_balancer $@