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
  -p 8080:8080
  -p 8443:8443
  --link air_db_dev:air_db
"

CONTAINER_NAME="air"
REMOTE_CONSOLE_COMMAND="bin/air remote_console"

container_ctl $@
