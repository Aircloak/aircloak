#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d central /bin/bash -c "bin/central stop"
}

DOCKER_IMAGE=$(aircloak_image_name central)

# we'll copy over SSL cert and key so local docker container listens on HTTPS
cp -rp priv/config/*.pem ./local_docker_config/

DOCKER_START_ARGS="
  -v $(pwd)/local_docker_config:/runtime_config
  -p 7080:7080
  -p 7443:7443
  --link central_db_dev:central_db
"
if [ "$CENTRAL_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e CENTRAL_HOST_NAME=$CENTRAL_HOST_NAME"; fi

CONTAINER_NAME="central"
REMOTE_CONSOLE_COMMAND="bin/central remote_console"

container_ctl $@
