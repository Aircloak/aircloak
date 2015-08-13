#!/bin/bash

set -eo pipefail
cat <<EOF > /etc/confd/templates/balancer.tmpl
upstream frontend {
  server $HOST_IP:8080 weight=100;
  {{ range gets "/service_instances/frontends/*" }}
    {{ \$data := json .Value }}
    server {{ \$data.http_endpoint }};
  {{ end }}
}

upstream backend {
  server $HOST_IP:11000 weight=100;
  {{ range gets "/service_instances/backends/*" }}
    {{ \$data := json .Value }}
    server {{ \$data.http_endpoint }};
  {{ end }}
}

server {
  listen 8200;

  location / {
    proxy_pass http://frontend;
  }

  location /backend {
    proxy_pass http://backend/backend/;
  }

  location /air_sandbox {
    proxy_pass http://backend/;
  }
}
EOF