#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function add_local_hosts {
  . /aircloak/site/bin/set_etcd_port.sh prod

  for host in $(
    curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service/local_names |
    jq '.node.value' |
    sed s/\"//g |
    tr " " "\n"
  ); do
    echo "127.0.0.1 $host.air-local" >> /etc/hosts
  done
}

export HTTP_HOST_IP=${AIR_HOST_NAME:-"127.0.0.1"}
export AIR_SITE_ENV="prod"

# add_local_hosts

log "Booting container."

exec gosu deployer /aircloak/site/bin/air foreground
