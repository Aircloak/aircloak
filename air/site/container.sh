#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh
. ../config/config.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d air_insights /bin/bash -c "AIR_INSIGHTS_ENV='prod' bin/air stop"
}

DOCKER_IMAGE=$(aircloak_image_name air_insights)

# we'll copy over SSL cert and key so local docker container listens on HTTPS
cp -rp priv/config/*.pem ./local_docker_config/

DOCKER_START_ARGS="
  -v $(pwd)/local_docker_config:/runtime_config
  -p 9080:8080
  -p 9443:8443
"
if [ "$AIR_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"; fi

CONTAINER_NAME="air_insights"
REMOTE_CONSOLE_COMMAND="AIR_INSIGHTS_ENV='prod' bin/air remote_console"

container_ctl $@
