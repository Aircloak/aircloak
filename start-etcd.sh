#!/usr/bin/env bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Setup
# -------------------------------------------------------------------

if [ ! -f "etcd_values" ]; then
  log "Please copy etcd_values.sample to etcd_values and edit as you see fit"
  exit 1
fi

if [ -n "$(env | grep boot2docker)" ]; then
  log "Assuming using boot2docker due to environment variables"
  ETCD_DEFAULT_IP=$(boot2docker ip)
else
  ETCD_DEFAULT_IP="127.0.0.1"
fi

export ETCD_PORT=${ETCD_PORT:-4001}
export HOST_IP=${ETCD_HOST_IP:-$ETCD_DEFAULT_IP}
export ETCD=$HOST_IP:$ETCD_PORT


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

docker stop etcd_air || true
docker rm etcd_air || true

# Start etcd for configuration management
log "Starting etcd"
docker run --name etcd_air -d -p 4001:4001 -p 2380:2380 -p 2379:2379 \
  quay.io/coreos/etcd:v2.0.6 \
  -name etcd0 \
  -advertise-client-urls http://${ETCD_DEFAULT_IP}:2379,http://${ETCD_DEFAULT_IP}:4001 \
  -listen-client-urls http://0.0.0.0:2379,http://0.0.0.0:4001 \
  -initial-advertise-peer-urls http://${ETCD_DEFAULT_IP}:2380 \
  -listen-peer-urls http://0.0.0.0:2380 \
  -initial-cluster-token etcd-cluster-1 \
  -initial-cluster etcd0=http://${ETCD_DEFAULT_IP}:2380 \
  -initial-cluster-state new

function etcd_is_up {
  curl --silent http://$ETCD/version > /dev/null
  if [ $? -ne 0 ]; then
    return 1
  else
    return 0
  fi
}

function etcd_set {
  path=$1
  value=$2
  log "Setting etcd: $path = $value"
  curl -XPUT -L --silent http://$ETCD/v2/keys$path -d value="$value" > /dev/null
}

# Crudely spin-lock, waiting for etcd to become available
until etcd_is_up; do
  log "etcd not yet running..."
  sleep 0.1
done
log "Etcd is running"

log "Creating required ETCD values for development"
. etcd_values