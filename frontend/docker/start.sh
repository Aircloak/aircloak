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

# Responsible for periodic registration of this frontend to etcd
function register_frontend {
  AIR_HOST_NAME=${AIR_HOST_NAME:-"127.0.0.1"}
  key="service_instances/frontends/$AIR_HOST_NAME"
  value="{\"http_endpoint\":\"$AIR_HOST_NAME:$(get_tcp_port prod air_frontend/http)\"}"
  while true; do
    curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/$key -XPUT -d value="$value" -d ttl=60 > /dev/null
    sleep 45
  done
}

function migrate_db_if_needed {
  last_performed_migration=$(
        curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service/frontend/last_performed_migration |
        jq --raw-output '.node.value'
      )
  most_recent_migration=$(ls -t1 db/migrate/ | tail -n 1)

  if [ "$last_performed_migration" != "$most_recent_migration" ]; then
    log "Migrating database"
    RAILS_ENV=production gosu deployer bundle exec rake db:migrate

    # Cache in etcd that the database has been migrated
    curl -XPUT -L \
        http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service/frontend/last_performed_migration \
        -d value="$most_recent_migration"
  fi
}

. $(dirname ${BASH_SOURCE[0]})/config.sh
export ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
log "Booting container. Expecting etcd at http://127.0.0.1:$ETCD_CLIENT_PORT."

add_local_hosts

migrate_db_if_needed

cat /tmp/nginx.conf \
  | sed "s/\$AIR_FRONTEND_HTTP_PORT/$(tcp_port 'air_frontend/http')/" \
  > /aircloak/nginx.conf


# Start unicorn in the background (but not as daemon). This ensures that its output
# still goes to stdout, while not blocking the main process.
log "Starting unicorn"
gosu deployer bundle exec unicorn -c config/unicorn.rb -E production &

register_frontend&

log "Starting nginx"
exec /usr/sbin/nginx -c /aircloak/nginx.conf
