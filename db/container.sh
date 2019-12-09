#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

cd ..

mkdir -p $(pwd)/db/configs
cp -rp air/priv/config/dev.json $(pwd)/db/configs/air_dev.json
cp -rp air/priv/config/test.json $(pwd)/db/configs/air_test.json
cp -rp central/priv/config/dev.json $(pwd)/db/configs/central_dev.json
cp -rp central/priv/config/test.json $(pwd)/db/configs/central_test.json

DB_PORT=${DB_PORT:-20002}
DOCKER_IMAGE="aircloak/aircloak_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_DATA=${DOCKER_DATA:-$HOME/.aircloak}
DOCKER_START_ARGS="
  -p $DB_PORT:5432
  -v $DOCKER_DATA/docker_volumes/aircloak_dev_db:/var/lib/postgresql/data
  -v $(pwd)/db/configs:/runtime_config
"

CONTAINER_NAME="aircloak_dev_db"

container_ctl $@
