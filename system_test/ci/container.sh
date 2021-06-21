#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh system_test

function prepare_for_nightly {
  local container_name="$1"

  # starting databases first, so they have the time to boot and initialize before client containers are started
  start_air_db $container_name
  start_cloak_dbs $container_name

  start_air_container $container_name
  start_cloak_container $container_name
}

function start_air_container {
  local container_name="$1"

  start_supporting_container $container_name air \
    -v $(pwd)/system_test/air_config:/runtime_config \
    -v $(pwd)/system_test/scripts:/aircloak/system_test/scripts \
    aircloak/air:$(git_head_image_tag)

  echo "waiting for air to start ..."
  wait_for_app "$container_name" air

  echo "configuring air"
  elixir_rcp $container_name air "Air.Repo.Seeder.seed()"
  admin_token=$(docker exec ${container_name}_air cat /aircloak/air/lib/air-$(cat VERSION)/priv/dev/admin_token)
  echo "$admin_token" > system_test/priv/dev/admin_token
}

function start_air_db {
  local container_name=$1
  start_supporting_container $container_name air_db \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk -e POSTGRES_DB=air \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    postgres:9.4 -c "listen_addresses=*"
}

function start_cloak_dbs {
  local container_name=$1

  start_supporting_container $container_name cloak_postgres \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    postgres:9.4 -c "listen_addresses=*" > /dev/null
}

function start_cloak_container {
  local container_name="$1"

  # note: we're making a temp copy of config, because we're recreating config files after generating compliance data
  mkdir -p $(pwd)/system_test/tmp/
  if [ -d $(pwd)/system_test/tmp/cloak_config ]; then rm -rf $(pwd)/system_test/tmp/cloak_config; fi
  cp -rp $(pwd)/system_test/cloak_config $(pwd)/system_test/tmp/

  start_supporting_container $container_name cloak \
    -v $(pwd)/system_test/tmp/cloak_config:/runtime_config \
    -v $(pwd)/cloak/priv/odbc/drivers:/odbc_drivers \
    -e GLOBAL_DB_NAMESPACE=$GLOBAL_DB_NAMESPACE \
    aircloak/cloak:$(git_head_image_tag)

  echo "waiting for cloak to start ..."
  wait_for_app $container_name cloak

  echo "populating database ..."
  elixir_rcp $container_name cloak "Cloak.SystemTest.Setup.run()"

  # using a dummy sleep to make sure async datasource reloading has finished
  sleep 10
}

function start_supporting_container {
  local container_name=$1
  local supporting_container_role=$2
  shift 2 || true

  local supporting_container_name="${container_name}_${supporting_container_role}"

  ensure_supporting_container $supporting_container_name $@
  docker network connect --alias $supporting_container_role $container_name $supporting_container_name
}

function wait_for_app {
  local container=$1
  local app=$2

  local timeout_prog=""

  if [ -x "$(command -v timeout)" ]; then timeout_prog=timeout; fi
  if [ -x "$(command -v gtimeout)" ]; then timeout_prog=gtimeout; fi
  if [ "$timeout_prog" == "" ]; then
    echo "Can't find timeout program! If you're on macOS, run `brew install coreutils`."
    exit 1
  fi

  $timeout_prog 1m system_test/ci/wait_for_app.sh "$container" "$app" || {
    local app_container="${container}_${app}"
    printf "\n\n\nTimeout waiting for $app! Docker log of the container:\n\n"
    docker logs $app_container
    exit 1
  }
}

function elixir_rcp {
  local container=$1
  local app=$2
  shift 2 || true

  docker exec ${container}_${app} /aircloak/$app/bin/$app rpc "$@"
}

mkdir -p system_test/priv/dev

mount_to_aircloak VERSION
mount_to_component config lib priv test mix.exs mix.lock Makefile .gitignore check_warnings.sh .formatter.exs
mount_cached_component deps _build .bash_history

if [ "$GLOBAL_DB_NAMESPACE" == "" ]; then
  if [ "$MPI" == "true" ]; then
    export GLOBAL_DB_NAMESPACE="mpi_system_test"
  else
    echo "
      Please set the GLOBAL_DB_NAMESPACE OS variable. If you're developing locally, then use a unique repeatable
      value, such as your first name. This value will be used as a prefix of the table names in the database.
    "

    exit 1
  fi
fi

function handle_build_image {
  build_base_images
  build_builder_images
  build_releases
  build_release_images
  build_component_image
}

function build_builder_images {
  PREVENT_OLD_IMAGE_REMOVAL=true cloak/build-image.sh builder_image
  PREVENT_OLD_IMAGE_REMOVAL=true air/build-image.sh builder_image
}

function build_releases {
  exec_with_retry "cloak/build-image.sh release"
  exec_with_retry "air/build-image.sh release"
}

function build_release_images {
  cloak/build-image.sh release_image
  air/build-image.sh release_image
}

function exec_with_retry {
  if ! eval "$@"; then
    sleep 5
    eval "$@"
  fi
}

case "$1" in
  build_image)
    handle_build_image
    ;;

  prepare_for_nightly)
    prepare_for_nightly $2
    ;;

  *)
    default_handle "$@"
    ;;
esac
