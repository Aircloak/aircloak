#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

docker build -t aircloak/air_db:latest .

if ! latest_version_running air_db; then
  stop_named_container air_db
  ./ensure_persistent_volume.sh

  ./container.sh start
  docker exec air_db /init_db.sh
  ./container.sh stop
fi
