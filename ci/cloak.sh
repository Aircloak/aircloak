#!/bin/bash

set -eo pipefail

. docker/docker_helper.sh

export CLOAK_NETWORK_ID=$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')
export DEFAULT_SAP_HANA_SCHEMA="TEST_SCHEMA_$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')"


function cleanup {
  set +x
  echo "destroying network"

  for container_id in $(
    docker network inspect $CLOAK_NETWORK_ID --format '
      {{range $key, $value := .Containers}}
        {{println $key}}
      {{end}}
    '
  ); do
    docker network disconnect $CLOAK_NETWORK_ID $container_id > /dev/null
  done

  docker network prune -f > /dev/null

  docker kill $CLOAK_CONTAINER > /dev/null
  docker rm $CLOAK_CONTAINER > /dev/null

  dangling_volumes=$(docker volume ls -qf dangling=true)
  if [ "$dangling_volumes" != "" ]; then
    docker volume rm $dangling_volumes > /dev/null
  fi
}

function ensure_container {
  container_name=$1
  shift

  if ! named_container_running $container_name ; then
    if [ ! -z "$(docker ps -a --filter=name=$container_name | grep -w $container_name)" ]; then
      echo "removing dead container $container_name"
      docker rm $container_name > /dev/null
    fi

    echo "starting container $container_name"
    docker run --detach --name $container_name $@ > /dev/null
  fi

  docker network connect --alias $container_name $CLOAK_NETWORK_ID $container_name
}

function start_network_container {
  docker run --detach --network=$CLOAK_NETWORK_ID $@ > /dev/null
}

function ensure_database_containers {
  ensure_container postgres9.4 postgres:9.4
  ensure_container mongo3.0 mongo:3.0
  ensure_container mongo3.2 mongo:3.2
  ensure_container mongo3.4 mongo:3.4
  ensure_container mysql5.7 -e MYSQL_ALLOW_EMPTY_PASSWORD=true mysql:5.7.19

  ensure_container sqlserver2017 -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest
}

function build_cloak_image {
  pushd ./cloak && make odbc_drivers && popd
  common/docker/elixir/build-image.sh
  build_aircloak_image cloak_ci ci/cloak.dockerfile ci/.cloak.dockerignore
}

function start_cloak_container {
  build_cloak_image

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

  export CLOAK_CONTAINER=$(
    docker run -d --network=$CLOAK_NETWORK_ID $mounts -e CLOAK_DATA_SOURCES="$CLOAK_DATA_SOURCES" \
      aircloak/cloak_ci:latest sleep infinity
  )
}

function start_cloak_with_databases {
  docker network create --driver bridge $CLOAK_NETWORK_ID > /dev/null
  trap cleanup EXIT TERM INT

  ensure_database_containers
  start_cloak_container
  run_in_cloak "mix deps.get"
}

function run_in_cloak {
  docker exec -it $CLOAK_CONTAINER /bin/bash -c ". ~/.asdf/asdf.sh && $@"
}
