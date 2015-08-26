#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function add_local_hosts {
  for host in $(
    curl -s -L http://127.0.0.1:$ETCD_PORT/v2/keys/service/local_names |
    jq '.node.value' |
    sed s/\"//g |
    tr " " "\n"
  ); do
    echo "127.0.0.1 $host.air-local" >> /etc/hosts
  done
}

export HTTP_HOST_IP=${AIR_HOST_NAME:-"127.0.0.1"}
export ETCD_PORT=${ETCD_PORT:-4002}

add_local_hosts

log "Booting container. Expecting etcd at http://127.0.0.1:$ETCD_PORT."

exec gosu deployer /aircloak/app/bin/air foreground
