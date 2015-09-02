#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh
. ../config/config.sh


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

if [ "$AIR_ENV" != "prod" ]; then
  stop_named_container etcd_air_dev
  stop_named_container etcd_air_test
fi

init_env prod
DOCKER_START_ARGS=$(docker_start_args)
container_ctl etcd_air $@

if [ "$1" = "start" ] || [ "$1" = "ensure_started" ] || [ "$1" = "console" ]; then
  wait_for_etcd prod

  if [ "$AIR_ENV" = "prod" ]; then
    log "Configuring ETCD for production"
    ./config_prod.sh
  else
    ./config_docker.sh

    log "Starting dev container"
    ./run-dev-container.sh

    log "Starting test container"
    ./run-test-container.sh
  fi
fi