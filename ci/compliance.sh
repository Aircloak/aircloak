#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

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
  echo "starting database containers"

  start_network_container --network-alias=postgresql \
    postgres:9.4

  start_network_container --network-alias=mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=true \
    mysql:5.7.19

  start_network_container --network-alias=mssql -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest
}

function build_cloak_image {
  pushd ./cloak && make odbc_drivers && popd
  common/docker/elixir/build-image.sh
  build_aircloak_image cloak_ci ci/cloak.dockerfile ci/.cloak.dockerignore
}

function run_in_cloak {
  docker run --rm -it --network=$NETWORK_ID aircloak/cloak_ci:latest \
    bash -c \
      "
        cp priv/config/compliance_ci.json priv/config/compliance.json &&
        cp priv/config/compliance_ci.json priv/config/test.json &&
        . ~/.asdf/asdf.sh &&
        $@
      "
}

export NETWORK_ID=$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')
docker network create --driver bridge $NETWORK_ID > /dev/null
trap destroy_network EXIT TERM INT

start_supporting_containers
build_cloak_image

run_in_cloak "
  mix ci.init_data_sources compliance &&
  mix gen.test_data compliance 10 &&
  mix test --only compliance --max-cases 10
"
