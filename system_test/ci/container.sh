#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh system_test

function prepare_for_test {
  local container_name="$1"

  if [ "$SKIP_DOCKER_BUILD" != "true" ]; then
    PREVENT_OLD_IMAGE_REMOVAL=true cloak/build-image.sh
    PREVENT_OLD_IMAGE_REMOVAL=true air/build-image.sh
  fi

  start_air_db $container_name
  start_air_container $container_name

  start_cloak_dbs $container_name
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
    {ok, User} = 'Elixir.Air.Service.User':login(<<\"admin@aircloak.com\">>, <<\"1234\">>),
    {ok, PrivacyPolicy} = 'Elixir.Air.Service.PrivacyPolicy':get(),
    'Elixir.Air.Service.User':'accept_privacy_policy!'(User, PrivacyPolicy),

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
    -e "DEFAULT_SAP_HANA_SCHEMA=TEST_SCHEMA_$container_name" \
    aircloak/cloak:$(git_head_image_tag)

  echo "waiting for cloak to start ..."
  wait_for_app $container_name cloak

  echo "populating database ..."
  erlang_eval $container_name cloak "
    'Elixir.Compliance':initialize(<<\"config\">>, 10, 1),
    'Elixir.Compliance':regenerate_config_from_db(<<\"config\">>)
  "
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

  while [ "$(docker exec ${container}_${app} /aircloak/$app/bin/$app ping)" != "pong" ]; do sleep 1; done
  until $(app_running $container $app); do sleep 1; done
}

function app_running {
  local container=$1
  local app=$2

  local running=$(erlang_eval $container $app "[App || {App, _, _} <- application:which_applications(), App =:= $app]")
  if [ "$running" == "[]" ]; then return 1; else return 0; fi
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

case "$1" in
  prepare_for_test)
    prepare_for_test $2
    ;;

  *)
    default_handle "$@"
    ;;
esac
