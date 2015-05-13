#!/bin/bash

# Taken from https://www.digitalocean.com/community/tutorials/how-to-use-confd-and-etcd-to-dynamically-reconfigure-services-in-coreos
#
set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export ETCD_PORT=${ETCD_PORT:-2379}
export HOST_IP=${HOST_IP:-172.17.42.1}
export ETCD=http://$HOST_IP:$ETCD_PORT

log "Booting container. Expecting etcd at $ETCD."

config="database"

log "Starting unicorn"
bundle exec unicorn -c config/unicorn.rb -D -E production

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Started."

tail -f log/*