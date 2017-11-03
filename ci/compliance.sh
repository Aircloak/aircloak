#!/bin/bash

set -eox pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

function destroy_network {
  set +x
  echo "destroying network"

  for container_id in $(
    docker network inspect $NETWORK_ID --format '
      {{range $key, $value := .Containers}}
        {{println $key}}
      {{end}}
    '
  ); do
    docker kill $container_id > /dev/null
  done

  docker network rm $NETWORK_ID > /dev/null
}

function start_network_container {
  docker run --detach --network=$NETWORK_ID $@ > /dev/null
}

function start_supporting_containers {
  start_network_container --network-alias=postgresql \
    postgres:9.4

  start_network_container --network-alias=mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=true \
    mysql:5.7.19

  start_network_container --network-alias=mssql -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest
}

export NETWORK_ID=$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')
docker network create --driver bridge $NETWORK_ID > /dev/null
trap destroy_network EXIT TERM INT

start_supporting_containers
