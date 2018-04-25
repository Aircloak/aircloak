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
  container_script air wait_for_service localhost:8080

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

case "$1" in
  start)
    start_system
    ;;

  stop)
    stop_system
    ;;

  *)
    echo "Invalid parameter"
    exit 1
    ;;
esac
