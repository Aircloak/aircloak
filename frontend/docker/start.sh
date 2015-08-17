#!/bin/bash

# Taken from https://www.digitalocean.com/community/tutorials/how-to-use-confd-and-etcd-to-dynamically-reconfigure-services-in-coreos
#
set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# Get the IP of the host. See:
#   https://groups.google.com/forum/#!msg/coreos-dev/fnMeC4B0pSc/adYRzDDoK1wJ
#   http://blog.famzah.net/2011/09/06/get-default-outgoing-ip-address-and-interface-on-linux/
export HOST_IP=$(ip route get 8.8.8.8 | grep via | awk '{print $3}')
export ETCD_HOST=${ETCD_HOST:-$HOST_IP}
export ETCD_PORT=${ETCD_PORT:-4002}

echo "$HOST_IP backend.local" >> /etc/hosts

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

config="database"

log "Starting nginx"
/usr/sbin/nginx -c /etc/nginx/nginx.conf

log "Starting unicorn"
# Exec ensures that unicorn replaces this process. This allows us to use
# docker to send signals to the unicorn process, and ultimately enables
# graceful termination of the process.
exec gosu deployer bundle exec unicorn -c config/unicorn.rb -E production
