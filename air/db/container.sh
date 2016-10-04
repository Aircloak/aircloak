#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh

cd ../..

if [ "$DB_ENV" == "dev" ] || [ "$DB_ENV" == "test" ]; then
  db_port=$(cat air/priv/config/$DB_ENV.json | jq --raw-output ".database.port")
  mkdir -p $(pwd)/air/db/configs/$DB_ENV
  cp -rp air/priv/config/$DB_ENV.json $(pwd)/air/db/configs/$DB_ENV/config.json
fi

DOCKER_IMAGE="aircloak/air_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_START_ARGS="
  -p $db_port:5432
  -v /docker_volumes/air_db_$DB_ENV:/var/lib/postgresql/data
  -v $(pwd)/air/db/configs/$DB_ENV:/runtime_config
"

CONTAINER_NAME="air_db"
if [ "$DB_ENV" != "" ]; then
  CONTAINER_NAME="${CONTAINER_NAME}_${DB_ENV}"
fi

container_ctl $@
