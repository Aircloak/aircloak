#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh integration_tests

function start_postgres_container {
  local owner_container_name=$1
  local postgres_version=$2

  local postgres_container_name="${owner_container_name}_postgres${postgres_version}"

  docker run \
    --detach --name "$postgres_container_name" \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:$postgres_version > /dev/null

  docker network connect --alias "postgres${postgres_version}" $owner_container_name $postgres_container_name
}

function prepare_for_test {
  start_postgres_container $1 "9.4"
  start_postgres_container $1 "9.5"
}

mount_to_aircloak VERSION RELEASE_EXPIRY_DATE common/elixir air cloak central
mount_to_component .gitignore config lib test mix.exs mix.lock
mount_cached_component deps _build .bash_history

case "$1" in
  prepare_for_test)
    shift
    prepare_for_test $1
    ;;

  *)
    default_handle "$@"
    ;;
esac
