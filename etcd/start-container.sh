#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh

# -------------------------------------------------------------------
# Setup
# -------------------------------------------------------------------

if [ ! -f "etcd_values" ]; then
  log "Please copy etcd_values.sample to etcd_values and edit as you see fit"
  exit 1
fi

init_env


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

docker stop etcd_air || true
docker rm etcd_air || true

# Start etcd for configuration management
log "Starting etcd"
docker run --name etcd_air -d -p ${ETCD_PORT}:${ETCD_PORT} -p 2380:2380 -p 2379:2379 \
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

log "Creating required ETCD values for local development"
. etcd_values_dev

./run-test-container.sh
