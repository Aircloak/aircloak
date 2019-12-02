#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

cd ..

if [ "$DB_ENV" == "dev" ] || [ "$DB_ENV" == "test" ]; then
  mkdir -p $(pwd)/db/configs
  cp -rp air/priv/config/$DB_ENV.json $(pwd)/db/configs/air_$DB_ENV.json
  cp -rp air/priv/config/test.json $(pwd)/db/configs/air_test.json
  cp -rp central/priv/config/dev.json $(pwd)/db/configs/central_dev.json
  cp -rp central/priv/config/test.json $(pwd)/db/configs/central_test.json
fi

DB_PORT=${DB_PORT:-20002}
DOCKER_IMAGE="aircloak/aircloak_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_DATA=${DOCKER_DATA:-$HOME/.aircloak}
DOCKER_START_ARGS="
  -p $DB_PORT:5432
  -v $DOCKER_DATA/docker_volumes/aircloak_db_$DB_ENV:/var/lib/postgresql/data
  -v $(pwd)/db/configs:/runtime_config
"

CONTAINER_NAME="aircloak_db"
if [ "$DB_ENV" != "" ]; then
  CONTAINER_NAME="${CONTAINER_NAME}_${DB_ENV}"
fi

container_ctl $@
