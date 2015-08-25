#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# Get the IP of the host. See:
#   https://groups.google.com/forum/#!msg/coreos-dev/fnMeC4B0pSc/adYRzDDoK1wJ
#   http://blog.famzah.net/2011/09/06/get-default-outgoing-ip-address-and-interface-on-linux/
export HOST_IP=$(ip route get 8.8.8.8 | grep via | awk '{print $3}')
export HTTP_HOST_IP=${AIR_HOST_NAME:-$HOST_IP}
export ETCD_HOST=${ETCD_HOST:-$HOST_IP}
export ETCD_PORT=${ETCD_PORT:-4002}

echo "$HOST_IP frontend.air-local" >> /etc/hosts

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

exec gosu deployer /aircloak/app/bin/air foreground
