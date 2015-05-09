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

log "One time setup of database.yml"
# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node $ETCD -config-file /etc/confd/conf.d/database.toml; do
  log "Waiting for confd to create initial database.yml configuration."
  sleep 5
done

log "Starting unicorn"
bundle exec unicorn -c config/unicorn.rb -D -E production

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Started."

tail -f log/*