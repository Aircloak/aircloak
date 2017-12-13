#!/bin/bash

set -eo pipefail

. docker/docker_helper.sh

export CLOAK_NETWORK_ID=$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')
export DEFAULT_SAP_HANA_SCHEMA="TEST_SCHEMA_$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')"

function ignore_cleanup {
  # NOOP which ignores cleanup while we're already cleaning up.
  :
}

function cleanup {
  local exit_status=$?

  # We'll ignore subsequent exit signals to avoid reentrancy.
  trap ignore_cleanup EXIT TERM INT

  if [ "$CLOAK_CONTAINER" != "" ]; then
    # This handles permission problems on Linux systems, when running as a non-root. Inside the docker container
    # we're giving ownership to the entire cloak folder to the same user as the user running the image. This ensures
    # that the user will own generated files/folders (e.g. _build and deps) on the host machine.
    # Without this trickery, the owner would always be root.
    # See [this issue](https://github.com/moby/moby/issues/2259) for more details.
    run_in_cloak "chown -R $UID /aircloak/cloak" || true

    # We need to kill the container before disconnecting it from the network. Otherwise, we end up with dangling network
    # connections on database containers.
    docker kill $CLOAK_CONTAINER > /dev/null || true
    docker rm $CLOAK_CONTAINER > /dev/null || true
  fi

  if [ "$POSTGRESQL_CONTAINER" != "" ]; then
    docker kill $POSTGRESQL_CONTAINER > /dev/null || true
    docker rm $POSTGRESQL_CONTAINER > /dev/null || true
  fi

  if [ "$CLOAK_NETWORK_ID" != "" ]; then
    echo "destroying network $CLOAK_NETWORK_ID"

    for container_id in $(
      docker network inspect $CLOAK_NETWORK_ID --format '
        {{range $key, $value := .Containers}}
          {{println $key}}
        {{end}}
      '
    ); do
      docker network disconnect $CLOAK_NETWORK_ID $container_id > /dev/null
    done

    docker network rm $CLOAK_NETWORK_ID > /dev/null
    echo "network $CLOAK_NETWORK_ID destroyed"
  fi

  local dangling_volumes=$(docker volume ls -qf dangling=true)
  if [ "$dangling_volumes" != "" ]; then
    docker volume rm $dangling_volumes > /dev/null
  fi

  exit $exit_status
}

function ensure_container {
  local container_name=$1
  shift

  if ! named_container_running $container_name ; then
    if [ ! -z "$(docker ps -a --filter=name=$container_name | grep -w $container_name)" ]; then
      echo "removing dead container $container_name"
      docker rm $container_name > /dev/null
    fi

    echo "starting container $container_name"
    docker run --detach --name $container_name $@ > /dev/null
  fi

  docker network connect --alias $container_name $CLOAK_NETWORK_ID $container_name
}

function start_network_container {
  docker run --detach --network=$CLOAK_NETWORK_ID $@ > /dev/null
}

function ensure_database_containers {
  ensure_container postgres9.4 postgres:9.4
  ensure_container mongo3.0 mongo:3.0
  ensure_container mongo3.2 mongo:3.2
  ensure_container mongo3.4 mongo:3.4
  ensure_container mysql5.7 -e MYSQL_ALLOW_EMPTY_PASSWORD=true mysql:5.7.19 --character-set-server=utf8mb4 --collation-server=utf8mb4_unicode_ci

  ensure_container sqlserver2017 -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=7fNBjlaeoRwz*zH9' \
    microsoft/mssql-server-linux:2017-latest
}

function build_cloak_image {
  pushd ./cloak && make odbc_drivers && popd

  # Only build image if it doesn't exist. Since images are tagged with the git sha, the existing image is exactly
  # the one we need.
  if [ "$(docker images | grep $(git_head_image_tag))" == "" ]; then
    common/docker/elixir/build-image.sh
    build_aircloak_image ci_cloak ci/docker/cloak.dockerfile ci/docker/.cloak.dockerignore
  fi
}

function start_cloak_container {
  mkdir -p tmp/ci/cloak

  if [ ! -f tmp/ci/cloak/.bash_history ]; then
    touch tmp/ci/cloak/.bash_history
  fi

  mounted_from_root="VERSION common/elixir"
  mounted_from_cloak="config datagen include lib perftest priv rel test mix.exs mix.lock Makefile"
  mounted_from_cloak_cache="deps _build .bash_history"

  local mounts="-v $(pwd)/tmp/ci/cloak/.bash_history:/root/.bash_history"

  for path in $mounted_from_root; do
    mounts="$mounts -v $(pwd)/$path:/aircloak/$path"
  done

  for path in $mounted_from_cloak; do
    mounts="$mounts -v $(pwd)/cloak/$path:/aircloak/cloak/$path"
  done

  for path in $mounted_from_cloak_cache; do
    mounts="$mounts -v $(pwd)/tmp/ci/cloak/$path:/aircloak/cloak/$path"
  done

  export CLOAK_CONTAINER=$(
    docker run -d --network=$CLOAK_NETWORK_ID $mounts -e CLOAK_DATA_SOURCES="$CLOAK_DATA_SOURCES" \
      aircloak/ci_cloak:$(git_head_image_tag) sleep infinity
  )
}

function create_network {
  echo "creating network $CLOAK_NETWORK_ID"
  docker network create --driver bridge $CLOAK_NETWORK_ID > /dev/null
  trap cleanup EXIT TERM INT
}

function start_cloak_with_databases {
  create_network
  ensure_database_containers
  start_cloak_container
  run_in_cloak "mix deps.get"
}

function run_in_cloak {
  docker exec $DOCKER_EXEC_ARGS -i -e DEFAULT_SAP_HANA_SCHEMA="$DEFAULT_SAP_HANA_SCHEMA" $CLOAK_CONTAINER \
    /bin/bash -c ". ~/.asdf/asdf.sh && $@"
}

function gen_test_data {
  local num_users=${COMPLIANCE_USERS:-50}
  run_in_cloak "MIX_ENV=test mix gen.test_data dockerized_ci $num_users"
}

function cloak_compliance {
  local concurrency=${COMPLIANCE_CONCURRENCY:-10}
  start_cloak_with_databases
  gen_test_data
  run_in_cloak "mix test --only compliance --max-cases $concurrency"
}

function debug_cloak_compliance {
  start_cloak_with_databases
  gen_test_data
  DOCKER_EXEC_ARGS="-t" run_in_cloak "/bin/bash"
}

function run_in_cloak_test {
  create_network
  CLOAK_DATA_SOURCES="postgresql9.4" start_cloak_container
  export POSTGRESQL_CONTAINER=$(docker run --detach postgres:9.4)
  docker network connect --alias postgres9.4 $CLOAK_NETWORK_ID $POSTGRESQL_CONTAINER
  for cmd in "$@"; do
    echo "--------------"
    echo "invoking $cmd"
    run_in_cloak "$cmd"
    echo ""
  done
}
