#!/bin/bash

set -e

cd $(dirname $0)

function log {
  echo "[aircloak] $1"
}

log "Starting DB"
db/container.sh start

log "Starting ECDF"
etcd/container.sh start

nginx/stop_nginx.sh
log "Starting nginx (http on port 5500 (hello), https on port 5600 (api))"
nginx -c $PWD/nginx/nginx.conf &

log "OK. Ready set go! Use ./nginx/stop-nginx.sh to stop nginx"
