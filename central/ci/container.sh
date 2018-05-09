#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh central

function prepare_for_test {
  container_name=$1
  postgres_container_name="${container_name}_postgres"

  docker run \
    --detach --name "$postgres_container_name" \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:9.5 > /dev/null

  docker network connect --alias postgres9.5 $container_name $postgres_container_name

  mongo_container_name="${container_name}_mongo"

  docker run \
    --detach --name "$mongo_container_name" \
    --tmpfs=/data/db \
    mongo:3.6.4 > /dev/null

  docker network connect --alias mongo3.6.4 $container_name $mongo_container_name
}

mount_to_aircloak VERSION common/elixir
mount_to_component \
  .gitignore assets config lib priv rel test mix.exs mix.lock Makefile README.md check_warnings.sh .formatter.exs
mount_cached_component deps _build .bash_history docs/_book docs/node_modules priv/static

case "$1" in
  prepare_for_test)
    shift
    prepare_for_test $1
    ;;

  *)
    default_handle "$@"
    ;;
esac
