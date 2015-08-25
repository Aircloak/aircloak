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

AIR_ROUTERS=${AIR_ROUTERS:-$HOST_IP}

function upstreams {
  for server in $(echo $AIR_ROUTERS | tr ";" "\n"); do
    printf "server $server:$1;\\\n"
  done
}

cat /aircloak/balancer/nginx.conf.tmpl \
  | sed "s#\$HTTPS_UPSTREAMS#$(upstreams 8200)#; s#\$HTTP_UPSTREAMS#$(upstreams 8201)#;" \
  > /aircloak/balancer/nginx.conf

log "Starting nginx"
exec /usr/local/nginx/sbin/nginx -c "/aircloak/balancer/nginx.conf"
