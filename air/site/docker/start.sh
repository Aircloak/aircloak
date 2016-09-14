#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function add_local_hosts {
  . /aircloak/insights/bin/set_etcd_port.sh prod

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
export AIR_INSIGHTS_ENV="prod"

# add_local_hosts

log "Booting container."
mkdir -p /aircloak/insights/lib/air-0.0.1/priv/config/
cp -rp /runtime_config/* /aircloak/insights/lib/air-0.0.1/priv/config/
exec gosu deployer /aircloak/insights/bin/air foreground
