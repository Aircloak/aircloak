#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

stop_named_container air_db

docker build -t aircloak/air_db:latest .
./ensure_persistent_volume.sh


./container.sh start
docker exec air_db /init_db.sh
./container.sh stop
