#!/bin/bash

# Taken from https://www.digitalocean.com/community/tutorials/how-to-use-confd-and-etcd-to-dynamically-reconfigure-services-in-coreos
#
set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export ETCD_HOST=${ETCD_HOST:-172.17.42.1}
export ETCD_PORT=${ETCD_PORT:-4002}

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

config="database"

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Starting unicorn"
# Exec ensures that unicorn replaces this process. This allows us to use
# docker to send signals to the unicorn process, and ultimately enables
# graceful termination of the process
exec bundle exec unicorn -c config/unicorn.rb -E production
