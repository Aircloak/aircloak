#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh

cd ../..

if [ "$DB_ENV" == "dev" ] || [ "$DB_ENV" == "test" ]; then
  db_port=$(cat central/priv/config/$DB_ENV.json | jq --raw-output ".database.port")
  mkdir -p $(pwd)/central/db/configs/$DB_ENV
  cp -rp central/priv/config/$DB_ENV.json $(pwd)/central/db/configs/$DB_ENV/config.json
fi

DOCKER_IMAGE="aircloak/central_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_START_ARGS="
  -p $db_port:5432
  -v /docker_volumes/central_db_$DB_ENV:/var/lib/postgresql/data
  -v $(pwd)/central/db/configs/$DB_ENV:/runtime_config
"

CONTAINER_NAME="central_db"
if [ "$DB_ENV" != "" ]; then
  CONTAINER_NAME="${CONTAINER_NAME}_${DB_ENV}"
fi

container_ctl $@
