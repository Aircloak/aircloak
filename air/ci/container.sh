#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

function build_image {
  common/docker/phoenix/build-image.sh
  build_aircloak_image ci_air air/ci/dockerfile air/ci/.dockerignore
}

function is_image_built {
  if [ "$(docker images -q aircloak/ci_air:$(git_head_image_tag))" == "" ]; then
    echo "no"
  else
    echo "yes"
  fi
}

function start_container {
  container_name=$1

  mkdir -p tmp/ci/air

  if [ ! -f tmp/ci/air/.bash_history ]; then
    touch tmp/ci/air/.bash_history
  fi

  mounted_from_root="VERSION RELEASE_EXPIRY_DATE common/elixir"
  mounted_from_air=".flowconfig .gitignore assets config datagen docs include lib perftest priv rel test mix.exs mix.lock Makefile README.md"
  mounted_from_air_cache="deps _build .bash_history docs/_book docs/node_modules priv/static"

  local mounts="-v $(pwd)/tmp/ci/air/.bash_history:/root/.bash_history"

  for path in $mounted_from_root; do
    mounts="$mounts -v $(pwd)/$path:/aircloak/$path"
  done

  for path in $mounted_from_air; do
    mounts="$mounts -v $(pwd)/air/$path:/aircloak/air/$path"
  done

  for path in $mounted_from_air_cache; do
    mounts="$mounts -v $(pwd)/tmp/ci/air/$path:/aircloak/air/$path"
  done

  docker network create --driver bridge $container_name > /dev/null

  docker run -d --name $container_name --network=$container_name $mounts $DOCKER_ARGS \
    -e DEFAULT_SAP_HANA_SCHEMA="TEST_SCHEMA_$container_name" \
    aircloak/ci_air:$(git_head_image_tag) sleep 3600 > /dev/null
}

function prepare_for_test {
  container_name=$1
  postgres_container_name="${container_name}_postgres"
  docker run --detach --name "$postgres_container_name" postgres:9.4 > /dev/null
  docker network connect --alias postgres9.4 $container_name $postgres_container_name
}

function prepare_for_compliance {
  container_name=$1
  ensure_database_containers

  for db_container in postgres9.4 mongo3.0 mongo3.2 mongo3.4 mysql5.7 sqlserver2017; do
    echo $db_container
    docker network connect --alias $db_container $container_name $db_container
  done
}

function ensure_container {
  local container_name=$1
  shift

  if ! named_container_running $container_name ; then
    if [ ! -z "$(docker ps -a --filter=name=$container_name | grep -w $container_name)" ]; then
      echo "removing dead container $container_name"
      docker rm $container_name > /dev/null
    fi

    echo "starting container $container_name"
    docker run --detach --name $container_name $@ > /dev/null
  fi
}

function run_in_container {
  air_container=$1
  shift || true

  cmd=". ~/.asdf/asdf.sh && $@"
  docker exec $DOCKER_EXEC_ARGS -i $air_container /bin/bash -c "$cmd"
}

function ensure_database_containers {
  ensure_container postgres9.4 postgres:9.4
  ensure_container mongo3.0 mongo:3.0
  ensure_container mongo3.2 mongo:3.2
  ensure_container mongo3.4 mongo:3.4
  ensure_container mysql5.7 -e MYSQL_ALLOW_EMPTY_PASSWORD=true mysql:5.7.19 --character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci

  ensure_container sqlserver2017 -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest
}

command=$1
shift || true

case "$command" in
  build_image)
    build_image
    ;;

  is_image_built)
    is_image_built
    ;;

  start_container)
    start_container $@
    ;;

  prepare_for_test)
    prepare_for_test $@
    ;;

  run_in_container)
    run_in_container $@
    ;;

  *)
    echo "invalid args: $command $@"
    exit 1
    ;;
esac
