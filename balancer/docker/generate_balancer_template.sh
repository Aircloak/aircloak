#!/bin/bash

set -eo pipefail
cat <<EOF > /etc/confd/templates/balancer.tmpl
upstream frontend {
  server $HOST_IP:8080 weight=100;
  {{ range gets "/service_instances/frontends/*" }}
    {{ \$data := json .Value }}
    server {{ \$data.http_endpoint }};
  {{ end }}

  keepalive 16;
}

upstream backend {
  server $HOST_IP:11000 weight=100;
  {{ range gets "/service_instances/backends/*" }}
    {{ \$data := json .Value }}
    server {{ \$data.http_endpoint }};
  {{ end }}

  keepalive 16;
}

server {
  listen 8200;

  gzip  on;
  gzip_disable "MSIE [1-6]\.(?!.*SV1)";
  gzip_comp_level 6;
  gzip_min_length 4096;
  gzip_proxied any;
  gzip_types text/plain application/json text/xml application/xml text/csv application/javascript text/css;

  location / {
    client_max_body_size 4G;
    keepalive_timeout 10;

    proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;

    proxy_read_timeout 150s;

    # enable this if you forward HTTPS traffic to unicorn,
    # this helps Rack set the proper URL scheme for doing redirects:
    proxy_set_header X-Forwarded-Proto \$scheme;

    # pass the Host: header from the client right along so redirects
    # can be set properly within the Rack application
    proxy_set_header Host \$http_host;

    # we don't want nginx trying to do something clever with
    # redirects, we set the Host: header above already.
    proxy_redirect off;

    proxy_pass http://frontend;
  }

  location /backend {
    proxy_pass http://backend/backend/;
    proxy_http_version 1.1;
    proxy_set_header Connection "";
  }

  location /air_sandbox {
    proxy_pass http://backend/;
    proxy_http_version 1.1;
    proxy_set_header Connection "";
  }
}
EOF