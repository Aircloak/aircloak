#!/bin/bash

cat <<-EOF > user-data
#cloud-config

---
coreos:
  etcd2:
    discovery: https://discovery.etcd.io/df692a97fdfa404321ab3040a5c67f0e
    advertise-client-urls: http://\$public_ipv4:2379,http://\$public_ipv4:4001
    initial-advertise-peer-urls: http://\$public_ipv4:2380
    listen-client-urls: http://0.0.0.0:2379,http://0.0.0.0:4001
    listen-peer-urls: http://\$public_ipv4:2380
  fleet:
    public-ip: \$public_ipv4
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