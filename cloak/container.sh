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
DOCKER_START_ARGS="--net=host -v $(pwd)/runtime_config:/aircloak/cloak/lib/cloak-0.1.0/priv/config"
REMOTE_CONSOLE_COMMAND="bin/cloak remote_console"

container_ctl $@
