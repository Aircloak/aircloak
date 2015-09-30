#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ./config.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function generate_haproxy_config {
  haproxy_config > /aircloak/balancer/haproxy.cfg
}

function haproxy_config {
  routers=$(routers)

  cat /aircloak/balancer/haproxy.cfg.tmpl \
  | sed "s#\$BALANCER_HTTPS_PORT#$(get_tcp_port prod balancer/https)#" \
  | sed "s#\$BALANCER_HTTP_PORT#$(get_tcp_port prod balancer/http)#" \
  | sed "s#\$HTTPS_UPSTREAMS#$(upstreams "$routers" $(get_tcp_port prod router/proxy_https))#" \
  | sed "s#\$HTTP_UPSTREAMS#$(upstreams "$routers" $(get_tcp_port prod router/proxy_http))#"
}

function routers {
  if [ -f /aircloak/balancer/config/routers ]; then
    cat /aircloak/balancer/config/routers
  else
    echo "127.0.0.1"
  fi
}

function upstreams {
  index=1
  while read router; do
    if [ "$router" != "" ]; then
      printf "server router_$index $router:$2 check send-proxy\\\n"
      index=$(($index+1))
    fi
  done < <(echo "$1")
}

function haproxy_config_reloader {
  while true; do
    current_config=$(cat /aircloak/balancer/haproxy.cfg)
    new_config=$(haproxy_config)

    if [ "$new_config" != "$current_config" ]; then
      echo "regenerating haproxy configuration"
      generate_haproxy_config

      {
        haproxy -c -f /aircloak/balancer/haproxy.cfg && \
            haproxy -f /aircloak/balancer/haproxy.cfg -sf $(pgrep haproxy)
      } ||
      {
        echo "Error in haproxy configuration -> skipping reload"
      }

    fi

    sleep 5
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

generate_haproxy_config

haproxy -f /aircloak/balancer/haproxy.cfg
wait_for_haproxy
log "haproxy started"

haproxy_config_reloader&

while haproxy_running; do
  sleep 5
done
log "Haproxy not running -> stopping the container"
