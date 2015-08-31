#!/bin/bash

. ../config/config.sh

ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
ETCD_PEER_PORT=$(get_tcp_port prod etcd/peer)

cat <<-EOF > user-data
#cloud-config

---
coreos:
  etcd2:
    discovery: https://discovery.etcd.io/df692a97fdfa404321ab3040a5c67f0e
    advertise-client-urls: http://\$public_ipv4:$ETCD_CLIENT_PORT
    initial-advertise-peer-urls: http://\$public_ipv4:$ETCD_PEER_PORT
    listen-client-urls: http://0.0.0.0:$ETCD_CLIENT_PORT
    listen-peer-urls: http://\$public_ipv4:$ETCD_PEER_PORT
  fleet:
    public-ip: \$public_ipv4
    etcd_servers: http://127.0.0.1:$ETCD_CLIENT_PORT
  units:
  - name: docker.service
    drop-ins:
    - name: 50-insecure-registry.conf
      content: |
        [Service]
        Environment=DOCKER_OPTS='--insecure-registry="$COREOS_HOST_IP:5000"'
  - name: etcd2.service
    command: start
  - name: fleet.service
    command: start
EOF