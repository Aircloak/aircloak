#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh system_test

function build_production_images {
  if [ "$SKIP_DOCKER_BUILD" != "true" ]; then
    PREVENT_OLD_IMAGE_REMOVAL=true cloak/build-image.sh&
    PREVENT_OLD_IMAGE_REMOVAL=true air/build-image.sh&
    wait
  fi
}

function prepare_for_system_test {
  # We need to build the production images again, because the SHA might have changed since the images have been built.
  build_production_images

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
  erlang_eval $container_name air "
    'Elixir.Air.Repo.Seeder':seed(),

    ok = 'Elixir.Air.Service.PrivacyPolicy':set(<<\"privacy policy\">>),
    {ok, User} = 'Elixir.Air.Service.User':login(<<\"admin@aircloak.com\">>, <<\"password1234\">>),

    {ok, License} = file:read_file(code:priv_dir(air) ++ \"/config/license.lic\"),
    ok = 'Elixir.Air.Service.License':load(License)
  "

  admin_token=$(docker exec ${container_name}_air cat /aircloak/air/lib/air-$(cat VERSION)/priv/dev/admin_token)
  mkdir -p system_test/priv/dev
  echo "$admin_token" > system_test/priv/dev/admin_token
}

function start_air_db {
  local container_name=$1
  start_supporting_container $container_name air_db \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk -e POSTGRES_DB=air \
    postgres:9.4
}

function start_cloak_dbs {
  local container_name=$1

  start_supporting_container $container_name sqlserver2017 \
    -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest

  start_supporting_container $container_name cloak_postgres \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:9.4 > /dev/null
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
    -e "__AC__DEFAULT_SAP_HANA_SCHEMA__=TEST_SCHEMA_$container_name" \
    -e GLOBAL_DB_NAMESPACE=$GLOBAL_DB_NAMESPACE \
    aircloak/cloak:$(git_head_image_tag)

  echo "waiting for cloak to start ..."
  wait_for_app $container_name cloak

  echo "populating database ..."
  erlang_eval $container_name cloak "
    'Elixir.Compliance':initialize(<<\"config\">>, 10, 1),
    'Elixir.Compliance':regenerate_config_from_db(<<\"config\">>),
    'Elixir.Cloak.DataSource':reinitialize_all_data_sources(),
    sys:get_state('Elixir.Cloak.DataSource'),
    'Elixir.Supervisor':terminate_child('Elixir.Cloak.Supervisor', 'Elixir.Cloak.AirSocket.Supervisor'),
    'Elixir.Supervisor':restart_child('Elixir.Cloak.Supervisor', 'Elixir.Cloak.AirSocket.Supervisor'),
    ok
  "

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

function erlang_eval {
  local container=$1
  local app=$2
  shift 2 || true

  docker exec ${container}_${app} /aircloak/$app/bin/$app eval $@
}

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

case "$1" in
  prepare_for_compile)
    build_production_images
    ;;

  prepare_for_system_test)
    prepare_for_system_test $2
    ;;

  *)
    default_handle "$@"
    ;;
esac
