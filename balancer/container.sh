#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh


STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30
BIND_IF=${BIND_IF:-"*"}
ROUTERS_FILE=${ROUTERS_FILE:-"routers"}

# Generate dummy config/routers if needed
mkdir -p config
if [ ! -f config/$ROUTERS_FILE ]; then
  echo "127.0.0.1" > config/$ROUTERS_FILE
fi

DOCKER_IMAGE="aircloak/air_balancer"
DOCKER_START_ARGS="
  --net=host
  -v $(pwd)/config:/aircloak/balancer/config
  -e BIND_IF=$BIND_IF
  -e ROUTERS_FILE=$ROUTERS_FILE
"

if [ "$BIND_IF" != "*" ]; then
  CONTAINER_NAME="air_balancer_$BIND_IF"
else
  CONTAINER_NAME="air_balancer"
fi
CONTAINER_ARGS="/aircloak/balancer/start.sh"

container_ctl $@