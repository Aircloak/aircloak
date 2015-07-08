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

DOCKER_START_ARGS="-p ${ETCD_PORT}:${ETCD_PORT} -p 2380:2380 -p 2379:2379 \
  quay.io/coreos/etcd:v2.0.6 \
  -name etcd0 \
  -advertise-client-urls http://${ETCD_DEFAULT_IP}:2379,http://${ETCD_DEFAULT_IP}:${ETCD_PORT} \
  -listen-client-urls http://0.0.0.0:2379,http://0.0.0.0:${ETCD_PORT} \
  -initial-advertise-peer-urls http://${ETCD_DEFAULT_IP}:2380 \
  -listen-peer-urls http://0.0.0.0:2380 \
  -initial-cluster-token etcd-cluster-1 \
  -initial-cluster etcd0=http://${ETCD_DEFAULT_IP}:2380 \
  -initial-cluster-state new"

container_ctl etcd_air $@

if [ "$1" = "start" ] || [ "$1" = "ensure_started" ] || [ "$1" = "console" ]; then
  # Crudely spin-lock, waiting for etcd to become available
  until etcd_is_up; do
    log "etcd not yet running..."
    sleep 0.1
  done
  log "Etcd is running"

  if [ "$AIR_ENV" = "prod" ]; then
    log "Configuring ETCD for production"
    ./config_prod.sh
  else
    ./config_local.sh

    log "Starting test container"
    ./run-test-container.sh
  fi
fi