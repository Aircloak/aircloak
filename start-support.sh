#!/bin/bash

set -e

function log {
  echo "[aircloak] $1"
}

function stop_nginx {
  nginx_pid="$(ps augx | grep nginx\ -c | grep -v grep | awk '{print $2}')"
  if [ -z "$nginx_pid" ]; then
    log "Nginx not running, not stopping"
  else
    log "Nginx is already running, killing it"
    kill "$nginx_pid"
  fi
}

log "Starting DB"
db/container.sh start

log "Starting ECDF"
etcd/container.sh start

stop_nginx
log "Starting nginx (on port 5500)"
cd nginx
nginx -c $PWD/nginx.conf &

log "OK. Ready set go! (Ctrl-C to end nginx)"

# Wait until user quits
tail -f /dev/null
