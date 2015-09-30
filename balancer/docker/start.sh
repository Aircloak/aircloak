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
  index=1
  for server in $(echo $AIR_ROUTERS | tr ";" "\n"); do
    printf "server router_$index $server:$1 check send-proxy\\\n"
    index=$(($index+1))
  done
}


# Helper functions for running haproxy daemon in the docker.
# We're running as daemon since it allows us to start multiple haproxy processes.
# To make it work with docker, we do following:
#   - keep this bash script running for as long as haproxy is running
#   - trap SIGQUIT signals
#   - on SIGQUIT, send SIGUSR1 to haproxy processes (soft stop)

function haproxy_running {
  pgrep haproxy > /dev/null
  return $?
}

function wait_for_haproxy {
  pgrep haproxy > /dev/null
  while [ ! haproxy_running ]; do
    echo "Waiting for haproxy to start..."
    sleep 1
  done
}

function soft_stop_haproxy {
  echo "Sending SIGUSR1 to haproxy"
  kill -SIGUSR1 $(pgrep haproxy)
}
trap soft_stop_haproxy SIGQUIT

# need syslog for logs
rsyslogd

cat /aircloak/balancer/haproxy.cfg.tmpl \
  | sed "s#\$BALANCER_HTTPS_PORT#$(get_tcp_port prod balancer/https)#" \
  | sed "s#\$BALANCER_HTTP_PORT#$(get_tcp_port prod balancer/http)#" \
  | sed "s#\$HTTPS_UPSTREAMS#$(upstreams $(get_tcp_port prod router/proxy_https))#" \
  | sed "s#\$HTTP_UPSTREAMS#$(upstreams $(get_tcp_port prod router/proxy_http))#" \
  > /aircloak/balancer/haproxy.cfg

haproxy -f /aircloak/balancer/haproxy.cfg
wait_for_haproxy
log "haproxy started"

while haproxy_running; do
  sleep 5
done
log "Haproxy not running -> stopping the container"
