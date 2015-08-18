#!/bin/bash

set -eo pipefail

cd $(dirname $0)

function upstream_contents {
  paste -d " " \
    <(cat docker/nginx/sites/upstreams.tmpl | grep upstream | awk '{print $2}') \
    <(cat docker/nginx/sites/upstreams.tmpl | grep 'server $HOST_IP:' | awk '{print $2}' | sed s/\$HOST_IP://) \
    | while read upstream; do
        upstream_name=$(echo $upstream | awk '{print $1}')
        forward_port=$(echo $upstream | awk '{print $2}')
        cat <<EOF
          upstream $upstream_name {
            server 127.0.0.1:${forward_port};
          }
EOF
      done
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
    | sed "s#*:8200#*:8202#; s#*:8201#*:8203#; s#/etc/nginx/support#$(pwd)/nginx_local/support#; s#/aircloak/ca#$(pwd)/dev_cert#" \
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
  grep -nr server_name ./nginx_local | egrep -o '[a-zA-Z0-9]+\.local' | sort | uniq |
    while read alias; do
      if [ $(cat /etc/hosts | egrep -c "^127.0.0.1.*$alias$" || true) == "0" ]; then
        echo "Add following to your /etc/hosts:"
        echo "  127.0.0.1 $alias"
        echo
      fi
    done
}

nginx -c $(pwd)/nginx_local/nginx.conf -s stop > /dev/null 2>&1 || true

generate_nginx_conf
check_etc_hosts

nginx -c $(pwd)/nginx_local/nginx.conf

echo "You can access the site at:
  Frontend:
    http://frontend.local:8203
    https://frontend.local:8202

  API:
    http://api.local:8203
    https://api.local:8202
"
