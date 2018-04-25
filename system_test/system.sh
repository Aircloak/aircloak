#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

NETWORK_NAME="system_test"

function start_system {
  docker network create --driver bridge $NETWORK_NAME > /dev/null

  start_air_db
  start_air_container
  start_cloak_dbs
  start_cloak_container

  remove_old_git_head_image_tags "aircloak"
}

function start_system_container {
  local container_name="system_test_$1"; shift || true

  ensure_supporting_container $container_name $@
  docker network connect --alias system_test_air_db $NETWORK_NAME $container_name
}

function start_air_db {
  start_system_container air_db \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk -e POSTGRES_DB=air \
    postgres:9.4 > /dev/null
}

function start_air_container {
  if [ "$SKIP_BUILD" != "true" ]; then PREVENT_OLD_IMAGE_REMOVAL=true air/build-image.sh; fi

  start_system_container air \
    -v $(pwd)/system_test/air_config:/runtime_config \
    -v $(pwd)/system_test/scripts:/aircloak/system_test/scripts \
    -p 8080:8080 -p 8443:8443 -p 8432:8432 -p 8081:8081 \
    aircloak/air:latest

  echo "waiting for air to start ..."
  wait_for_app air

  echo "configuring air"
  erlang_eval air "
    'Elixir.Air.Repo.Seeder':seed(),

    ok = 'Elixir.Air.Service.PrivacyPolicy':set(<<\"privacy policy\">>),
    {ok, User} = 'Elixir.Air.Service.User':login(<<\"admin@aircloak.com\">>, <<\"1234\">>),
    {ok, PrivacyPolicy} = 'Elixir.Air.Service.PrivacyPolicy':get(),
    'Elixir.Air.Service.User':'accept_privacy_policy!'(User, PrivacyPolicy),

    {ok, License} = file:read_file(code:priv_dir(air) ++ \"/config/license.lic\"),
    ok = 'Elixir.Air.Service.License':load(License)
  "
}

function start_cloak_dbs {
  start_system_container cloak_postgres \
    --tmpfs=/ramdisk:rw,size=1G -e PGDATA=/ramdisk \
    postgres:9.4 > /dev/null
}

function start_cloak_container {
  if [ "$SKIP_BUILD" != "true" ]; then PREVENT_OLD_IMAGE_REMOVAL=true cloak/build-image.sh; fi

  # note: we're making a temp copy of config, because we're recreating config files after generating compliance data
  mkdir -p $(pwd)/system_test/tmp/
  if [ -d $(pwd)/system_test/tmp/cloak_config ]; then rm -rf $(pwd)/system_test/tmp/cloak_config; fi
  cp -rp $(pwd)/system_test/cloak_config $(pwd)/system_test/tmp/

  start_system_container cloak \
    -v $(pwd)/system_test/tmp/cloak_config:/runtime_config \
    aircloak/cloak:latest

  echo "waiting for cloak to start ..."
  wait_for_app cloak

  echo "populating database ..."
  erlang_eval cloak "
    'Elixir.Compliance':initialize(<<\"config\">>, 10, 1),
    'Elixir.Compliance':regenerate_config_from_db(<<\"config\">>)
  "
}

function stop_system {
  for container_id in $(docker ps -a --filter=name="system_test_" --format="{{.ID}}"); do
    docker kill $container_id > /dev/null || true
    docker rm $container_id > /dev/null || true
  done

  docker network rm $NETWORK_NAME > /dev/null || true
}

function container_script {
  local container=$1; shift || true
  local script=$1; shift || true

  docker exec "system_test_$container" /aircloak/system_test/scripts/$script.sh $@
}

function erlang_eval {
  local container=$1; shift || true
  docker exec "system_test_$container" /aircloak/$container/bin/$container eval $@
}

function wait_for_app {
  local app=$1
  while [ "$(docker exec system_test_$app /aircloak/$app/bin/$app ping)" != "pong" ]; do sleep 1; done
  until $(app_running $app); do sleep 1; done
}

function app_running {
  local app=$1
  local running=$(erlang_eval $app "[App || {App, _, _} <- application:which_applications(), App =:= $app]")
  if [ "$running" == "[]" ]; then return 1; else return 0; fi
}

case "$1" in
  start)
    start_system
    ;;

  stop)
    stop_system
    ;;

  restart)
    stop_system
    start_system
    ;;

  *)
    echo "Invalid parameter"
    exit 1
    ;;
esac
