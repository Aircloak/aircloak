#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
. ../docker/docker_helper.sh

./db/build-image.sh

echo "Starting databases for central"
DB_ENV="dev" ./db/container.sh ensure_started
DB_ENV="test" ./db/container.sh ensure_started
DOCKER_DATA=${DOCKER_DATA:-$HOME/.aircloak}

if ! named_container_running central_mongo ; then
  printf "Starting mongodb for central "
  docker run -d --name central_mongo \
    -p 27017:27017 \
    -v $DOCKER_DATA/docker_volumes/central_mongo_db:/data/db \
    mongo:3.6.4 > /dev/null
fi
