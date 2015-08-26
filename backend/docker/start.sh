#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export HTTP_HOST_IP=${AIR_HOST_NAME:-"127.0.0.1"}
export ETCD_HOST=${ETCD_HOST:-"127.0.0.1"}
export ETCD_PORT=${ETCD_PORT:-4002}

echo "127.0.0.1 frontend.air-local" >> /etc/hosts

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

exec gosu deployer /aircloak/app/bin/air foreground
