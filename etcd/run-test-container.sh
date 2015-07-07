#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh


# -------------------------------------------------------------------
# Setup
# -------------------------------------------------------------------

if [ -n "$(env | grep boot2docker)" ]; then
  log "Assuming using boot2docker due to environment variables"
  ETCD_DEFAULT_IP=$(boot2docker ip)
else
  ETCD_DEFAULT_IP="127.0.0.1"
fi

export ETCD_PORT=4003
export HOST_IP=${ETCD_HOST_IP:-$ETCD_DEFAULT_IP}
export ETCD=$HOST_IP:$ETCD_PORT


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

# Start etcd for configuration management
log "Starting etcd_air_test"
container_ctl etcd_air_test start -p ${ETCD_PORT}:${ETCD_PORT} \
  quay.io/coreos/etcd:v2.0.6 \
  -name etcd0 \
  -advertise-client-urls http://${ETCD_DEFAULT_IP}:2379,http://${ETCD_DEFAULT_IP}:${ETCD_PORT} \
  -listen-client-urls http://0.0.0.0:2379,http://0.0.0.0:${ETCD_PORT} \
  -initial-advertise-peer-urls http://${ETCD_DEFAULT_IP}:2380 \
  -listen-peer-urls http://0.0.0.0:2380 \
  -initial-cluster-token etcd-cluster-1 \
  -initial-cluster etcd0=http://${ETCD_DEFAULT_IP}:2380 \
  -initial-cluster-state new

# Crudely spin-lock, waiting for etcd to become available
until etcd_is_up; do
  log "etcd not yet running..."
  sleep 0.1
done
log "Etcd is running"

log "Creating required ETCD values for development"
. etcd_values_test
if [ -f local_settings/test ]; then
  . ./local_settings/test
fi