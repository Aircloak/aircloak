#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh

init_env


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

stop_named_container etcd_air_test

DOCKER_START_ARGS=$(docker_start_args "-p 2380:2380 -p 2379:2379")
container_ctl etcd_air $@

if [ "$1" = "start" ] || [ "$1" = "ensure_started" ] || [ "$1" = "console" ]; then
  # Crudely spin-lock, waiting for etcd to become available
  wait_for_etcd

  if [ "$AIR_ENV" = "prod" ]; then
    log "Configuring ETCD for production"
    ./config_prod.sh
  else
    ./config_local.sh

    log "Starting test container"
    ./run-test-container.sh
  fi
fi