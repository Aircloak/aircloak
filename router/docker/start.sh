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

function airpub_publish_allows {
  allows=$(
        curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service/airpub/allow_publish \
            | jq ".node.value" \
            | sed s/\"//g
      )
  while read -d " " allow; do
    if [ "$allow" != "" ]; then
      echo "allow $allow;"
    fi
  done < <(echo "$allows ")
}


. $(dirname ${BASH_SOURCE[0]})/config.sh
export ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
log "Booting container. Expecting etcd at http://127.0.0.1:$ETCD_CLIENT_PORT."

add_local_hosts

AIR_HOST_NAME=${AIR_HOST_NAME:-"127.0.0.1"}

cat /aircloak/router/docker/nginx/sites/upstreams.tmpl \
  | sed "s/\$AIR_HOST_NAME/$AIR_HOST_NAME/g; " \
  | sed "s/\$AIR_BACKEND_HTTP_PORT/$(tcp_port 'air_backend/http')/" \
  | sed "s/\$AIRPUB_HTTP_PORT/$(tcp_port 'airpub/http')/" \
  | sed "s/\$AIR_FRONTEND_HTTP_PORT/$(tcp_port 'air_frontend/http')/" \
  > /etc/confd/templates/upstreams.tmpl

# Ensure root keys exist (equivalent of mkdir -p)
curl -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service_instances/frontends -XPUT -d dir="true"
curl -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/service_instances/backends -XPUT -d dir="true"

cp -rp /aircloak/router/docker/nginx.toml /etc/confd/conf.d/

# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/nginx.toml; do
  echo "[nginx] waiting for confd to create initial nginx configuration"
  sleep 5
done

# Put a continual polling `confd` process into the background to watch
# for changes every 10 seconds
confd -interval 10 -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/nginx.toml &
log "confd is now monitoring etcd for changes..."

mkdir -p /etc/nginx/support
cp -rp /aircloak/router/docker/nginx/support/* /etc/nginx/support

for config in $(ls -1 /aircloak/router/docker/nginx/sites/*.conf); do
  cat $config \
  | sed "s#\$ROUTER_HTTPS_PORT#$(tcp_port router/https)#" \
  | sed "s#\$ROUTER_HTTP_PORT#$(tcp_port router/http)#" \
  > /etc/nginx/conf.d/$(basename $config)
done

generate_local_http_allows
echo "$(airpub_publish_allows)" > /etc/nginx/support/airpub_publish_allows.conf

log "Starting nginx"
exec nginx -g "daemon off;"
