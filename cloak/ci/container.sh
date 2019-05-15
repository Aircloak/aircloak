#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh cloak

function prepare_for_test {
  container_name=$1
  postgres_container_name="${container_name}_postgres"

  docker run \
    --detach --name "$postgres_container_name" \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:9.6 > /dev/null

  docker network connect --alias postgres9.6 $container_name $postgres_container_name
}

function prepare_for_compliance {
  container_name=$1
  ensure_database_containers

  drill_container_name="${container_name}_drill1.15"
  docker run \
    --detach --name $drill_container_name -it \
    -v $(pwd)/cloak/ci/data:/tmp/data \
    harisekhon/apache-drill:1.15

  docker network connect --alias drill1.15 $container_name $drill_container_name

  for db_container in postgres9.6 mongo3.4 mysql5.7 sqlserver2017 oracle11g; do
    echo $db_container
    docker network connect --alias $db_container $container_name $db_container
  done
}

function ensure_database_containers {
  ensure_supporting_container postgres9.6 --tmpfs=/ramdisk:rw,size=2G -e PGDATA=/ramdisk postgres:9.6
  ensure_supporting_container mongo3.4 --tmpfs=/data/db:rw,size=4G mongo:3.4
  ensure_supporting_container mysql5.7 --tmpfs=/var/lib/mysql:rw,size=2G \
    -e MYSQL_ALLOW_EMPTY_PASSWORD=true mysql:5.7.19 \
    --character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci

  ensure_supporting_container sqlserver2017 -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=Sql{}server1' \
    microsoft/mssql-server-linux:2017-latest

  ensure_supporting_container oracle11g -e ORACLE_DISABLE_ASYNCH_IO=true wnameless/oracle-xe-11g
}

mount $(ci_tmp_folder)/cloak/.cargo /root/.cargo
mount_to_aircloak VERSION common/elixir bom
mount_to_component \
  ci/data config datagen include lib src perftest priv rel test mix.exs mix.lock Makefile check_warnings.sh .formatter.exs
mount_cached_component deps _build .bash_history priv/odbc/drivers priv/native

case "$1" in
  prepare_for_test)
    prepare_for_test $2
    ;;

  prepare_for_compliance)
    prepare_for_compliance $2
    ;;

  start_container)
    container_name="$2"
    push_docker_arg "--tmpfs=/data/db:rw,size=1G"
    default_handle "$@"
    ;;

  run_in_container)
    container_name="$2"
    push_docker_arg "-e CLOAK_DATA_SOURCES=\"$CLOAK_DATA_SOURCES\""
    default_handle "$@"
    ;;

  ensure_database_containers)
    ensure_database_containers
    ;;

  *)
    default_handle "$@"
    ;;
esac
