#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

export NETWORK_ID=$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')
export MSSQL_TEMP_FOLDER="$(mktemp -d)"
export DEFAULT_SAP_HANA_SCHEMA="TEST_SCHEMA_$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')"


function cleanup {
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
    docker rm $container_id > /dev/null
  done

  dangling_volumes=$(docker volume ls -qf dangling=true)
  if [ "$dangling_volumes" != "" ]; then
    docker volume rm $dangling_volumes > /dev/null
  fi

  docker network prune -f > /dev/null
  rm -rf $MSSQL_TEMP_FOLDER
}

function start_network_container {
  docker run --detach --network=$NETWORK_ID $@ > /dev/null
}

function start_supporting_containers {
  echo "starting database containers"

  start_network_container --network-alias=postgresql \
    postgres:9.4

  start_network_container --network-alias=mongo3.0 \
    mongo:3.0

  start_network_container --network-alias=mongo3.2 \
    mongo:3.2

  start_network_container --network-alias=mongo3.4 \
    mongo:3.4

  start_network_container --network-alias=mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=true \
    mysql:5.7.19

  if [ $(uname -s) != "Darwin" ]; then
    # On Linux, we have to mount mssql folder because otherwise mssql container is not stoppable.
    mount_opt="-v $MSSQL_TEMP_FOLDER:/var/opt/mssql";
  else
    # On macos mssql container doesn't work with mounted folder. At the same time the container is stoppable :-)
    # Therefore, we're not mounting a folder here.
    mount_opt="";
  fi
  start_network_container --network-alias=sqlserver -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' $mount_opt \
    microsoft/mssql-server-linux:2017-latest
}

function build_cloak_image {
  pushd ./cloak && make odbc_drivers && popd
  common/docker/elixir/build-image.sh
  build_aircloak_image cloak_ci ci/cloak.dockerfile ci/.cloak.dockerignore
}

function run_in_cloak {
  mkdir -p tmp/ci/cloak

  if [ ! -f tmp/ci/cloak/.bash_history ]; then
    touch tmp/ci/cloak/.bash_history
  fi

  mounted_from_root="VERSION common/elixir"
  mounted_from_cloak="config datagen include lib perftest priv rel test mix.exs mix.lock Makefile"
  mounted_from_cloak_cache="deps _build .bash_history"

  mounts="-v $(pwd)/tmp/ci/cloak/.bash_history:/root/.bash_history"

  for path in $mounted_from_root; do
    mounts="$mounts -v $(pwd)/$path:/aircloak/$path"
  done

  for path in $mounted_from_cloak; do
    mounts="$mounts -v $(pwd)/cloak/$path:/aircloak/cloak/$path"
  done

  for path in $mounted_from_cloak_cache; do
    mounts="$mounts -v $(pwd)/tmp/ci/cloak/$path:/aircloak/cloak/$path"
  done

  docker run --rm -it --network=$NETWORK_ID $mounts aircloak/cloak_ci:latest \
    bash -c \
      "
        . ~/.asdf/asdf.sh &&
        $@
      "
}

docker network create --driver bridge $NETWORK_ID > /dev/null
trap cleanup EXIT TERM INT

start_supporting_containers
build_cloak_image

run_in_cloak "
  export DEFAULT_SAP_HANA_SCHEMA='$DEFAULT_SAP_HANA_SCHEMA' &&
  mix deps.get &&
  MIX_ENV=test mix gen.test_data compliance_ci 10 &&
  mix test --only compliance --max-cases 4
"
