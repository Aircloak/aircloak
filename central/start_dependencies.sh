#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
. ../docker/docker_helper.sh

if ! named_container_running central_mongo ; then
  printf "Starting mongodb for central "
  DOCKER_DATA=${DOCKER_DATA:-$HOME/.aircloak}
  docker run -d --name central_mongo \
    -p 27017:27017 \
    -v $DOCKER_DATA/docker_volumes/central_mongo_db:/data/db \
    mongo:3.6.4 > /dev/null
fi
