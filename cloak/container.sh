#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d cloak /bin/bash -c "bin/cloak stop"
}

CONTAINER_NAME="cloak"
DOCKER_IMAGE=$(aircloak_image_name $CONTAINER_NAME)
DOCKER_START_ARGS="
  -v $(pwd)/local_docker_config:/runtime_config
  --link air_insights:air_insights
  --link air_db:air_db
"
REMOTE_CONSOLE_COMMAND="bin/cloak remote_console"

container_ctl $@
