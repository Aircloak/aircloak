#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
. ../docker/docker_helper.sh

if ! named_container_running aircloak_influxdb ; then
  printf "Starting InfluxDB "
  docker run -d --name aircloak_influxdb -p 8086:8086 influxdb:1.3.2 > /dev/null

  until $(curl -X POST --output /dev/null --silent --head --fail 'http://localhost:8086/query?q=create+database+performance'); do
    printf '.'
    sleep 1
  done
  printf "\n"
fi
echo "InfluxDB running"
