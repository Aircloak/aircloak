#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh
. ../config/config.sh

./ensure_persistent_volume.sh

DOCKER_START_ARGS="-v /docker_volumes/air_db:/var/lib/postgresql/data \
  -p $(get_tcp_port dev database/tcp):5432 \
  aircloak/air_db:latest"

container_ctl air_db $@
