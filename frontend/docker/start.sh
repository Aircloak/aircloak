#!/bin/bash

# Taken from https://www.digitalocean.com/community/tutorials/how-to-use-confd-and-etcd-to-dynamically-reconfigure-services-in-coreos
#
set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function add_local_hosts {
  for host in $(
    curl -s -L http://127.0.0.1:$ETCD_PORT/v2/keys/service/local_names |
    jq '.node.value' |
    sed s/\"//g |
    tr " " "\n"
  ); do
    echo "127.0.0.1 $host.air-local" >> /etc/hosts
  done
}

export ETCD_PORT=${ETCD_PORT:-4002}

add_local_hosts

log "Booting container. Expecting etcd at http://127.0.0.1:$ETCD_PORT."

config="database"

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Starting unicorn"
# Exec ensures that unicorn replaces this process. This allows us to use
# docker to send signals to the unicorn process, and ultimately enables
# graceful termination of the process.
exec gosu deployer bundle exec unicorn -c config/unicorn.rb -E production
