#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

# Override the generic function, since we need to perform special handling
function gracefully_stop_container {
  docker exec -d air_backend /bin/bash -c "/aircloak/app/bin/air stop"
}

DOCKER_START_ARGS="-p 11000:11000 -p 9000:9000 aircloak/air_backend:latest"
container_ctl air_backend $@
