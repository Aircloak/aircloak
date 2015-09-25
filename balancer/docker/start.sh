#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ./config.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

AIR_ROUTERS=${AIR_ROUTERS:-"127.0.0.1"}

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
