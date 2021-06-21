#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

trap cleanup EXIT TERM INT

function ignore_cleanup {
  # NOOP which ignores cleanup while we're already cleaning up.
  :
}

function cleanup {
  local exit_status=$?

  # We'll ignore subsequent exit signals to avoid reentrancy.
  trap ignore_cleanup EXIT TERM INT

  clean_dangling

  if [ "$KEEP_DB_CONTAINERS" != "true" ]; then
    # also stop named db containers so they don't waste dev resources
    for container in postgres9.6 quickstart.cloudera; do
      docker rm -f $container > /dev/null 2>&1 || true
    done
  fi

  exit $exit_status
}

function docker_script {
  cloak/ci/container.sh $@
}

function clean_dangling {
  printf "performing cleanup of dangling images..."

  for container in $(docker ps --format="{{.Names}}" --filter="name=local_ci_"); do
    docker rm -f $container > /dev/null 2>&1 || true
  done

  local dangling_volumes=$(docker volume ls -qf dangling=true)
  if [ "$dangling_volumes" != "" ]; then
    docker volume rm $dangling_volumes > /dev/null
  fi

  for network in $(docker network ls --format="{{.Name}}" --filter="name=local_ci_"); do
    for container_id in $(
      docker network inspect $network --format '{{range $key, $value := .Containers}} {{println $key}} {{end}}'
    ); do
      docker network disconnect $network $container_id > /dev/null
    done
    docker network rm $network > /dev/null || true
  done

  echo " done"
}

function start_compliance_container {
  clean_dangling

  container_id="local_ci_$(cat /dev/urandom | LC_ALL=C tr -dc 'a-zA-Z' | head -c 16; echo '')"

  docker_script build_image

  STOP_AFTER=infinity DOCKER_ARGS="-e GLOBAL_DB_NAMESPACE='$GLOBAL_DB_NAMESPACE'" docker_script start_container $container_id
  DOCKER_ARGS="-t" docker_script run_in_container $container_id MIX_ENV=test make
  DOCKER_ARGS="-t" docker_script prepare_for_compliance $container_id

  printf "\ngenerating users...\n"
  DOCKER_ARGS="-t" docker_script run_in_container $container_id \
    MIX_ENV=test mix gen.test_data dockerized_ci ${COMPLIANCE_USERS:-10}

  printf "\nyou can invoke compliance tests with \`mix test --only compliance\`\n\n"
  DOCKER_ARGS="-t" docker_script run_in_container $container_id "/bin/bash"
}

function start_dev_container {
  clean_dangling
  docker_script build_image

  container_id="local_ci_$(cat /dev/urandom | LC_ALL=C tr -dc 'a-zA-Z' | head -c 16; echo '')"
  STOP_AFTER=infinity docker_script start_container $container_id

  local oracle_container_id="${container_id}_oracle"
  docker run \
    -detached \
    --name $oracle_container_id \
    --network=container:$container_id \
    -e ORACLE_DISABLE_ASYNCH_IO=true \
    quay.io/aircloak/oracle-database:12.2.0.1-ee

  printf "\nyou can start the system with \`make start\`\n\n"

  CLOAK_DATA_SOURCES='oracle' \
  DOCKER_ARGS="-e DEPLOY_CONFIG='dev' -t" \
    docker_script run_in_container $container_id "/bin/bash"
}

case "$1" in
  "compliance")
    start_compliance_container
    ;;

  "dev")
    start_dev_container
    ;;

  *)
    echo "${BASH_SOURCE[0]} start_compliance | start_dev_container"
    exit 1
    ;;
esac
