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
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    postgres:9.6 > /dev/null

  docker network connect --alias postgres9.6 $container_name $postgres_container_name
}

function prepare_for_compliance {
  container_name=$1
  ensure_database_containers

  for db_container in oracle-db12ee postgres9.6 mongo3.6 mysql5.7 sqlserver2017 quickstart.cloudera; do
    echo $db_container
    docker network connect --alias $db_container $container_name $db_container
  done
}

function ensure_database_containers {
  ensure_supporting_container oracle-db12ee \
    -e ORACLE_PWD=oracle \
    --mount type=bind,src=$(pwd)/cloak/ci/init_oracle.sql,dst=/docker-entrypoint-initdb.d/setup/init_oracle.sql \
    --mount type=bind,src=$(pwd)/cloak/ci/oracle_udfs.sql,dst=/mnt/cloak/oracle_udfs.sql \
    quay.io/aircloak/oracle-database:12.2.0.1-ee

  ensure_supporting_container postgres9.6 --tmpfs=/ramdisk:rw,size=2G -e PGDATA=/ramdisk -e POSTGRES_HOST_AUTH_METHOD=trust postgres:9.6

  ensure_supporting_container mongo3.6 --tmpfs=/data/db:rw,size=4G mongo:3.6

  ensure_supporting_container mysql5.7 --tmpfs=/var/lib/mysql:rw,size=2G \
    -e MYSQL_ALLOW_EMPTY_PASSWORD=true mysql:5.7.19 \
    --character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci

  ensure_supporting_container sqlserver2017 -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=Sql{}server1' \
    microsoft/mssql-server-linux:2017-latest

  ensure_supporting_container quickstart.cloudera -it \
    --hostname quickstart.cloudera -p 21050:21050 \
    quay.io/aircloak/cloudera-quickstart-vm-5.13.0-0-beta \
    /usr/bin/docker-quickstart
}

mount $(pwd)/cloak/priv/odbc/drivers/cloudera /opt/cloudera
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
