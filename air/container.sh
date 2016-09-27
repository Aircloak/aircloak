#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d air /bin/bash -c "bin/air stop"
}

DOCKER_IMAGE=$(aircloak_image_name air)

# we'll copy over SSL cert and key so local docker container listens on HTTPS
cp -rp priv/config/*.pem ./local_docker_config/

DOCKER_START_ARGS="
  -v $(pwd)/local_docker_config:/runtime_config
  -p 9080:8080
  -p 9443:8443
  --link air_db_dev:air_db
"
if [ "$AIR_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"; fi

CONTAINER_NAME="air"
REMOTE_CONSOLE_COMMAND="bin/air remote_console"

container_ctl $@
