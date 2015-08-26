#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function generate_file {
  cat $1 |  > $2
}

function generate_local_http_allows {
  # We allow http access via *.air-local only to peer containers on the same host.
  mask=$(echo $1 | sed "s/\./ /g" | awk '{print $1"."$2"."0"."0}')
  cat <<EOF > /etc/nginx/support/local_http_allows.conf
    allow 127.0.0.1;
    deny all;
EOF
}

export ETCD_HOST=${ETCD_HOST:-"127.0.0.1"}
export ETCD_PORT=${ETCD_PORT:-4002}

AIR_HOST_NAME=${AIR_HOST_NAME:-"127.0.0.1"}

cat /aircloak/router/docker/nginx/sites/upstreams.tmpl \
  | sed "s/\$AIR_HOST_NAME/$AIR_HOST_NAME/g;" \
  > /etc/confd/templates/upstreams.tmpl

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

# Ensure root keys exist (equivalent of mkdir -p)
curl -L http://$ETCD_HOST:$ETCD_PORT/v2/keys/service_instances/frontends -XPUT -d dir="true"
curl -L http://$ETCD_HOST:$ETCD_PORT/v2/keys/service_instances/backends -XPUT -d dir="true"

cp -rp /aircloak/router/docker/nginx.toml /etc/confd/conf.d/

# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node $ETCD_HOST:$ETCD_PORT -config-file /etc/confd/conf.d/nginx.toml; do
  echo "[nginx] waiting for confd to create initial nginx configuration"
  sleep 5
done

# Put a continual polling `confd` process into the background to watch
# for changes every 10 seconds
confd -interval 10 -node $ETCD_HOST:$ETCD_PORT -config-file /etc/confd/conf.d/nginx.toml &
log "confd is now monitoring etcd for changes..."

mkdir -p /etc/nginx/support
cp -rp /aircloak/router/docker/nginx/support/* /etc/nginx/support
cp -rp /aircloak/router/docker/nginx/sites/*.conf /etc/nginx/conf.d/

generate_local_http_allows

log "Starting nginx"
exec nginx -g "daemon off;"
