#!/bin/bash

# Taken from https://www.digitalocean.com/community/tutorials/how-to-use-confd-and-etcd-to-dynamically-reconfigure-services-in-coreos
#
set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export ETCD_HOST=${ETCD_HOST:-172.17.42.1}
export ETCD_PORT=${ETCD_PORT:-4001}

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

config="database"

log "Starting unicorn"
bundle exec unicorn -c config/unicorn.rb -D -E production

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Started."

tail -f log/*