#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh
. ../config/config.sh

./ensure_persistent_volume.sh

DOCKER_IMAGE="aircloak/air_db"
DOCKER_IMAGE_VERSION="latest"
DOCKER_START_ARGS="
  -v /docker_volumes/air_db:/var/lib/postgresql/data
  -p $(get_tcp_port dev database/tcp):5432
"
CONTAINER_NAME="air_db"

container_ctl $@
