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
    curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service/local_names |
    jq '.node.value' |
    sed s/\"//g |
    tr " " "\n"
  ); do
    echo "127.0.0.1 $host.air-local" >> /etc/hosts
  done
}

function tcp_port {
  curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/tcp_ports/$1 \
      | jq ".node.value" \
      | sed s/\"//g
}

. $(dirname ${BASH_SOURCE[0]})/config.sh
export ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
log "Booting container. Expecting etcd at http://127.0.0.1:$ETCD_CLIENT_PORT."

add_local_hosts

log "Migrating database"
RAILS_ENV=production gosu deployer bundle exec rake db:migrate
RAILS_ENV=production gosu deployer bundle exec rake db:version

cat /tmp/nginx.conf \
  | sed "s/\$AIR_FRONTEND_HTTP_PORT/$(tcp_port 'air_frontend/http')/" \
  > /aircloak/nginx.conf

log "Starting nginx"
/usr/sbin/nginx -c /aircloak/nginx.conf

log "Starting unicorn"
# Exec ensures that unicorn replaces this process. This allows us to use
# docker to send signals to the unicorn process, and ultimately enables
# graceful termination of the process.
exec gosu deployer bundle exec unicorn -c config/unicorn.rb -E production
