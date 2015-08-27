#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../etcd/etcd_lib.sh
export ETCD="127.0.0.1:4003"

function upstream_contents {
  cat <<EOF
    upstream frontend {
      server 127.0.0.1:$(etcd_get /tcp_ports/air_frontend/http);
    }
    upstream backend {
      server 127.0.0.1:$(etcd_get /tcp_ports/air_backend/http);
    }
    upstream local_backend {
      server 127.0.0.1:$(etcd_get /tcp_ports/air_backend/http);
    }
    upstream aircloak {
      server 127.0.0.1:10000;
    }
    upstream aircloak_stage {
      server 127.0.0.1:10001;
    }
EOF
}

function generate_nginx_conf {
  mkdir -p nginx_local
  rm -rf nginx_local/*

  mkdir -p nginx_local/sites
  mkdir -p nginx_local/support

  for config in $(ls -1 docker/nginx/support/*.conf); do
    cat $config \
    | sed "s#/etc/nginx/support#$(pwd)/nginx_local/support#; s#/aircloak/ca#$(pwd)/dev_cert#" \
    > nginx_local/support/$(basename $config)
  done

  for config in $(ls -1 docker/nginx/sites/*.conf); do
    cat $config \
    | sed "s#\$ROUTER_HTTPS_PORT#$(etcd_get /tcp_ports/router/https)#" \
    | sed "s#\$ROUTER_HTTP_PORT#$(etcd_get /tcp_ports/router/http)#" \
    | sed "s#/etc/nginx/support#$(pwd)/nginx_local/support#; s#/aircloak/ca#$(pwd)/dev_cert#" \
    > nginx_local/sites/$(basename $config)
  done

  cat <<EOF > ./nginx_local/nginx.conf
    worker_processes  1;

    events {
      worker_connections  1024;
      multi_accept on;
    }

    http {
      $(upstream_contents)
      include $(pwd)/nginx_local/sites/*;
    }
EOF
}

function check_etc_hosts {
  missing_sites=$(
    grep -nr server_name ./nginx_local | egrep -o '[a-zA-Z0-9\-]+\.air\-local' | sort | uniq |
      while read alias; do
        if [ $(cat /etc/hosts | egrep -c "^127.0.0.1.*$alias$" || true) == "0" ]; then
          sites="$sites  127.0.0.1 $alias\n"
          echo "$sites"
        fi
      done | tail -n 1
  )

  if [[ ! -z $missing_sites ]]; then
    printf "You need to add following entries to your /etc/hosts:\n$missing_sites\n"
  fi
}

kill -2 $(ps aux | grep nginx | grep master | grep $(pwd)/nginx_local/nginx.conf | awk '{print $2}') > /dev/null 2>&1 || true

generate_nginx_conf

echo
check_etc_hosts

nginx -c $(pwd)/nginx_local/nginx.conf

echo "You can access following sites:
  https://frontend.air-local:$(etcd_get /tcp_ports/router/https)
  https://api.air-local:$(etcd_get /tcp_ports/router/https)
  https://infrastructure-api.air-local:$(etcd_get /tcp_ports/router/https)
  https://aircloak.air-local:$(etcd_get /tcp_ports/router/https)
"
