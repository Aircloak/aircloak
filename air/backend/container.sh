#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d air_backend /bin/bash -c "AIR_BACKEND_ENV='prod' /aircloak/app/bin/air stop"
}

DOCKER_IMAGE=$(aircloak_image_name air_backend)

DOCKER_START_ARGS="--net=host"
if [ "$AIR_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"; fi

CONTAINER_NAME="air_backend"
REMOTE_CONSOLE_COMMAND="AIR_BACKEND_ENV='prod' bin/air remote_console"

container_ctl $@
