#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ./docker/docker_helper.sh

# PostgreSQL Database
./db/build-image.sh

echo "Starting database"
./db/container.sh ensure_started

./air/ldap/build-image.sh

echo "Starting LDAP"
LDAP_ENV="dev" ./air/ldap/container.sh ensure_started
LDAP_ENV="test" ./air/ldap/container.sh ensure_started

if ! named_container_running central_mongo ; then
  printf "Starting mongodb for central "
  DOCKER_DATA=${DOCKER_DATA:-$HOME/.aircloak}
  docker run -d --name central_mongo \
    -p 27017:27017 \
    -v $DOCKER_DATA/docker_volumes/central_mongo_db:/data/db \
    mongo:3.6.4 > /dev/null
  printf "\n"
fi

if ! named_container_running aircloak_influxdb ; then
  printf "Starting InfluxDB "
  docker run -d --name aircloak_influxdb -p 8086:8086 influxdb:1.3.2 > /dev/null

  until $(curl -X POST --output /dev/null --silent --head --fail 'http://localhost:8086/query?q=create+database+performance'); do
    printf '.'
    sleep 1
  done
  printf "\n"
fi
