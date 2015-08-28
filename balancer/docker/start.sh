#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ./config.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# Get the IP of the host. See:
#   https://groups.google.com/forum/#!msg/coreos-dev/fnMeC4B0pSc/adYRzDDoK1wJ
#   http://blog.famzah.net/2011/09/06/get-default-outgoing-ip-address-and-interface-on-linux/
HOST_IP=$(ip route get 8.8.8.8 | grep via | awk '{print $3}')

AIR_ROUTERS=${AIR_ROUTERS:-$HOST_IP}

function upstreams {
  for server in $(echo $AIR_ROUTERS | tr ";" "\n"); do
    printf "server $server:$1;\\\n"
  done
}

cat /aircloak/balancer/nginx.conf.tmpl \
  | sed "s#\$BALANCER_HTTPS_PORT#$(get_tcp_port prod balancer/https)#" \
  | sed "s#\$BALANCER_HTTP_PORT#$(get_tcp_port prod balancer/http)#" \
  | sed "s#\$HTTPS_UPSTREAMS#$(upstreams $(get_tcp_port prod router/https))#" \
  | sed "s#\$HTTP_UPSTREAMS#$(upstreams $(get_tcp_port prod router/http))#" \
  > /aircloak/balancer/nginx.conf

log "Starting nginx"
exec /usr/local/nginx/sbin/nginx -c "/aircloak/balancer/nginx.conf"
