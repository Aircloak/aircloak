#!/bin/bash

set -eo pipefail

cd $(dirname $0)

function upstream_contents {
  paste -d " " \
    <(cat docker/upstreams.tmpl | grep upstream | awk '{print $2}') \
    <(cat docker/upstreams.tmpl | grep 'server $HOST_IP:' | awk '{print $2}' | sed s/\$HOST_IP://) \
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

function make_site_config {
  mkdir -p nginx_local/sites
  cat docker/$1.conf \
  | sed 's/listen \*:8200;/listen \*:8201;/; s/$AIR_DOMAIN/local/' \
  > nginx_local/sites/$1.conf
}

function generate_nginx_conf {
  mkdir -p nginx_local
  rm -rf nginx_local/*

  make_site_config default
  make_site_config frontend
  make_site_config backend

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

echo "You can access the site at http://frontend.local:8201"
