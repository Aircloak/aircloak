#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/..

. docker/docker_helper.sh

cd air/db

docker build -t aircloak/air_db:latest .

if ! latest_version_running air_db; then
  stop_named_container air_db
  ./ensure_persistent_volume.sh

  ./container.sh start
  docker exec air_db /init_db.sh
  ./container.sh stop
fi
