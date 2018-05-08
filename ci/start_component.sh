#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

CONTAINER_ID="ci_debug_$(cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z' | head -c 16; echo '')"

trap cleanup EXIT TERM INT

function ignore_cleanup {
  # NOOP which ignores cleanup while we're already cleaning up.
  :
}

function cleanup {
  local exit_status=$?

  # We'll ignore subsequent exit signals to avoid reentrancy.
  trap ignore_cleanup EXIT TERM INT
  remove_docker_containers
  exit $exit_status
}

function docker_script {
  local component=$1
  shift || true

  ./$component/ci/container.sh $@
}

function remove_docker_containers {
  printf "removing docker containers..."

  for container in $(docker ps --format="{{.Names}}" --filter="name=$CONTAINER_ID"); do
    docker rm -f $container > /dev/null || true
  done

  local dangling_volumes=$(docker volume ls -qf dangling=true)
  if [ "$dangling_volumes" != "" ]; then
    docker volume rm $dangling_volumes > /dev/null
  fi

  for container_id in $(
    docker network inspect $CONTAINER_ID --format '{{range $key, $value := .Containers}} {{println $key}} {{end}}'
  ); do
    docker network disconnect $container_id $CONTAINER_ID > /dev/null
  done
  docker network rm $CONTAINER_ID > /dev/null || true

  echo " done"
}

component=$1
if [ ! -e "$component/ci/container.sh" ]; then
  printf "\nUsage: $(basename "$0") component_name\n\n"
  exit 1
fi

docker_script $component build_image

docker_script $component start_container $CONTAINER_ID
docker_script $component prepare_for_test $CONTAINER_ID

DOCKER_ARGS="-t" docker_script $component run_in_container $CONTAINER_ID "/bin/bash"
