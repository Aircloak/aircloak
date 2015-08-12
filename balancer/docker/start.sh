#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# Get the IP of the host. See:
#   https://groups.google.com/forum/#!msg/coreos-dev/fnMeC4B0pSc/adYRzDDoK1wJ
#   http://blog.famzah.net/2011/09/06/get-default-outgoing-ip-address-and-interface-on-linux/
HOST_IP=$(ip route get 8.8.8.8 | grep via | awk '{print $3}')
export ETCD_HOST=${ETCD_HOST:-$HOST_IP}
export ETCD_PORT=${ETCD_PORT:-4002}

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node $ETCD_HOST:$ETCD_PORT -config-file /etc/confd/conf.d/nginx.toml; do
  echo "[nginx] waiting for confd to create initial nginx configuration"
  sleep 5
done

# Put a continual polling `confd` process into the background to watch
# for changes every 10 seconds
confd -interval 10 -node $ETCD_HOST:$ETCD_PORT -config-file /etc/confd/conf.d/nginx.toml &
log "confd is now monitoring etcd for changes..."

log "Starting nginx"
exec nginx -g "daemon off;"
