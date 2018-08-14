#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh air

function prepare_for_test {
  container_name=$1
  postgres_container_name="${container_name}_postgres"

  docker run \
    --detach --name "$postgres_container_name" \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:9.4 > /dev/null

  docker network connect --alias postgres9.4 $container_name $postgres_container_name

  air/ldap/build-image.sh
  ldap_container_name="${container_name}_ldap"

  docker run --detach \
    --name "${ldap_container_name}" \
    -e LDAP_TLS_VERIFY_CLIENT=allow \
    -e LDAP_TLS_CRT_FILENAME=localhost.crt \
    -e LDAP_TLS_KEY_FILENAME=localhost.key \
    -e LDAP_TLS_CA_CRT_FILENAME=ca.crt \
    aircloak/ldap:latest

  docker network connect --alias ldap.aircloak $container_name $ldap_container_name
}

mount_to_aircloak VERSION common/elixir bom
mount_to_component \
  .flowconfig .gitignore assets config datagen docs include lib perftest priv rel test mix.exs mix.lock Makefile \
  README.md check_warnings.sh .formatter.exs
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
