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

function etcd_get {
  curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/$1 \
      | jq --raw-output ".node.value"
}

function add_local_hosts {
  for host in $(
    etcd_get service/local_names |
    tr " " "\n"
  ); do
    echo "127.0.0.1 $host.air-local" >> /etc/hosts
  done
}

function tcp_port {
  etcd_get "tcp_ports/$1"
}

function allows {
  allows=$(etcd_get $1)
  while read -d " " allow; do
    if [ "$allow" != "" ] && [ "$allow" != "null" ]; then
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
  | sed "s/\$AIRPUB_HTTP_PORT/$(tcp_port 'airpub/http')/" \
  | sed "s/\$AIR_INSIGHTS_HTTP_PORT/$(tcp_port 'insights/http')/" \
  | sed "s#include /etc/nginx/support/upstream_keepalive.conf;#$(cat /aircloak/router/docker/nginx/support/upstream_keepalive.conf)#" \
  > /etc/confd/templates/upstreams.tmpl

cp -rp /aircloak/router/docker/nginx/support/maintenance.tmpl /etc/confd/templates/

# Ensure root keys exist (equivalent of mkdir -p)
curl -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/maintenance -XPUT -d dir="true"

cp -rp /aircloak/router/docker/conf.d/*.toml /etc/confd/conf.d/

mkdir -p /etc/nginx/support

# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/upstreams.toml; do
  echo "[nginx] waiting for confd to create initial upstreams configuration"
  sleep 5
done

# Try to make initial configuration every 5 seconds until successful
until confd -onetime -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/maintenance.toml; do
  echo "[nginx] waiting for confd to create initial maintenance configuration"
  sleep 5
done

# Put a continual polling `confd` process into the background to watch for changes
confd -interval 5 -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/upstreams.toml &
confd -interval 2 -node 127.0.0.1:$ETCD_CLIENT_PORT -config-file /etc/confd/conf.d/maintenance.toml &
log "confd is now monitoring etcd for changes..."

cp -rp /aircloak/router/docker/nginx.conf /etc/nginx
mv /aircloak/router/docker/nginx/support/* /etc/nginx/support/
mv /aircloak/router/docker/nginx/static /etc/nginx/

for config in $(ls -1 /aircloak/router/docker/nginx/sites/*.conf); do
  cat $config \
  | sed "s#\$INSIGHTS_SITE#$(etcd_get /site/insights)#" \
  | sed "s#\$API_SITE#$(etcd_get /site/api)#" \
  | sed "s#\$INFRASTRUCTURE_API_SITE#$(etcd_get /site/infrastructure_api)#" \
  | sed "s#\$AIRPUB_SITE#$(etcd_get /site/airpub)#" \
  | sed "s#\$AIRCLOAK_SITE#$(etcd_get /site/aircloak)#" \
  | sed "s#\$ROUTER_HTTPS_PORT#$(tcp_port router/https)#" \
  | sed "s#\$ROUTER_HTTP_PORT#$(tcp_port router/http)#" \
  | sed "s#\$ROUTER_PROXY_HTTPS_PORT#$(tcp_port router/proxy_https)#" \
  | sed "s#\$ROUTER_PROXY_HTTP_PORT#$(tcp_port router/proxy_http)#" \
  > /etc/nginx/conf.d/$(basename $config)
done

generate_local_http_allows
echo "$(allows service/airpub/allow_publish)" > /etc/nginx/support/airpub_publish_allows.conf
echo "$(allows service/infrastructure_api/allow)" > /etc/nginx/support/infrastructure_api_allows.conf

log "Starting nginx"
exec nginx -g "daemon off;"
