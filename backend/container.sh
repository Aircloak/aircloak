#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

REGISTRY_URL=${REGISTRY_URL:-""}

if [ "$REGISTRY_URL" != "" ]; then
  REGISTRY_URL="$REGISTRY_URL""/"
fi

if [ "$ETCD_PORT" != "" ]; then
  DOCKER_START_ARGS="$DOCKER_START_ARGS -e ETCD_PORT=$ETCD_PORT"
fi

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d air_backend /bin/bash -c "/aircloak/app/bin/air stop"
}

if [ "$AIR_HOST_NAME" != "" ]; then
  DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"
fi

if [ "$EXPORT_BEAM_PORTS" == "true" ]; then
  DOCKER_START_ARGS="$DOCKER_START_ARGS -p 4369:4369 -p 20000:20000"
fi

DOCKER_START_ARGS="$DOCKER_START_ARGS \
  -p 11000:11000 -p 9000:9000 \
  --net=host \
  "$REGISTRY_URL"aircloak/air_backend:latest
"
REMOTE_CONSOLE_COMMAND="bin/air remote_console"
container_ctl air_backend $@
