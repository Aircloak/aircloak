function cloud_config {
  export ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
  export ETCD_PEER_PORT=$(get_tcp_port prod etcd/peer)

  cat <<EOF
write_files:
  - path: /etc/environment
    permissions: 0644
    owner: root:root
    content: |
      COREOS_PUBLIC_IPV4=\$public_ipv4
      COREOS_PRIVATE_IPV4=\$private_ipv4

  - path: /aircloak/registry_v2_req
    permissions: 0755
    owner: root:root
    content: |
      protocol="http"
      if [ -e "\$HOME/.docker/config.json" ]; then
        token=\$(
              cat \$HOME/.docker/config.json |
              jq --raw-output ".auths[\"https://$REGISTRY_URL/v2/\"] | select(. != null) | .auth"
            )
        if [ "\$token" != "" ]; then
          protocol="https"
          auth_header="-H 'Authorization: Basic \$token'"
        fi
      fi

      eval "curl -s \$auth_header \$protocol://$REGISTRY_URL/v2/\$1"

coreos:
  etcd2:
    name: "\$public_ipv4"
    advertise-client-urls: http://\$public_ipv4:$ETCD_CLIENT_PORT
    initial-advertise-peer-urls: http://\$public_ipv4:$ETCD_PEER_PORT
    listen-client-urls: http://0.0.0.0:$ETCD_CLIENT_PORT
    listen-peer-urls: http://\$public_ipv4:$ETCD_PEER_PORT
    initial-cluster: $(initial_cluster)
    initial-cluster-state: ${INITIAL_CLUSTER_STATE:-new}
  fleet:
    public-ip: \$public_ipv4
    etcd_servers: http://127.0.0.1:$ETCD_CLIENT_PORT
  units:
  - name: docker.service
    drop-ins:
    - name: 50-insecure-registry.conf
      content: |
        [Service]
        Environment=DOCKER_OPTS='--insecure-registry="$REGISTRY_URL"'

  - name: etcd2.service
    command: start
  - name: fleet.service
    command: start

  - name: swap.service
    command: start
    content: |
      [Unit]
      Description=Turn on swap

      [Service]
      Type=oneshot
      Environment="SWAPFILE=/2GiB.swap"
      RemainAfterExit=true
      ExecStartPre=/bin/bash -c "\
        fallocate -l 2G \$SWAPFILE && \
        chmod 600 \$SWAPFILE && \
        chattr +C \$SWAPFILE && \
        mkswap \$SWAPFILE && \
        losetup -f \$SWAPFILE"
      ExecStart=/usr/bin/sh -c "/sbin/swapon \$(/usr/sbin/losetup -j \${SWAPFILE} | /usr/bin/cut -d : -f 1)"
      ExecStop=/usr/bin/sh -c "/sbin/swapoff \$(/usr/sbin/losetup -j \${SWAPFILE} | /usr/bin/cut -d : -f 1)"
      ExecStopPost=/usr/bin/sh -c "/usr/sbin/losetup -d \$(/usr/sbin/losetup -j \${SWAPFILE} | /usr/bin/cut -d : -f 1)"

      [Install]
      WantedBy=local.target

  - name: air-installer.service
    command: start
    content: |
      # For description of unit options, such as After, Requires, BindsTo, see
      # http://www.freedesktop.org/software/systemd/man/systemd.unit.html

      [Unit]
      Description=Air machine install
      After=docker.service
      After=etcd2.service

      [Service]
      Type=oneshot
      Environment="REGISTRY_URL=$REGISTRY_URL"
      ExecStart=$(install_command)
EOF
}

function initial_cluster {
  for ip in $MACHINES; do
    echo "$ip=http://$ip:$ETCD_PEER_PORT"
  done | paste -sd "," -
}

function run_command {
  cmd=$(tr '\n' ' ' < <(echo "$1"))
  echo "/bin/sh -c '$cmd'"
}

function install_command {
  run_command '
        latest_version=$(
              /aircloak/registry_v2_req aircloak/air_installer/tags/list |
              jq --raw-output ".tags | select(. != null) | .[]" |
              sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
              head -n 1
            ) &&
        until docker pull $REGISTRY_URL/aircloak/air_installer:$latest_version; do sleep 1; done &&
        installer_id=$(docker create $REGISTRY_URL/aircloak/air_installer:$latest_version) &&
        docker cp $installer_id:aircloak - > /tmp/aircloak.tar &&
        docker stop $installer_id &&
        docker rm -v $installer_id &&
        cd /tmp/ && tar -xf aircloak.tar -C / && rm aircloak.tar &&
        /aircloak/air/install/install.sh
      '
}
