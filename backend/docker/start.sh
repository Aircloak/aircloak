#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export ETCD_HOST=${ETCD_HOST:-172.17.42.1}
export ETCD_PORT=${ETCD_PORT:-4002}

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

/aircloak/app/bin/air foreground
