function cloud_config {
  export ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client)
  export ETCD_PEER_PORT=$(get_tcp_port prod etcd/peer)

  cat <<EOF
#cloud-config
    coreos:
      etcd2:
        name: "%m"
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
          [Unit]
          Description=Air machine install
          After=docker.service
          After=etcd2.service
          After=fleet.service
          Requires=docker.service
          Requires=etcd2.service
          Requires=fleet.service

          [Service]
          Type=oneshot
          RemainAfterExit=yes
          Environment="REGISTRY_URL=$REGISTRY_URL"
          ExecStart=$(install_command)

      - name: air-keys.service
        command: start
        content: |
          [Unit]
          Description=Air key checker

          [Service]
          Type=oneshot
          RemainAfterExit=yes
          ExecStart=$(air_key_command)
EOF
}

function initial_cluster {
  for machine in $MACHINES; do
    IFS="," read ip id <<< "$machine"
    echo "$id=http://$ip:$ETCD_PEER_PORT"
  done | paste -sd "," -
}

function run_command {
  cmd=$(tr '\n' ' ' < <(echo "$1"))
  echo "/bin/sh -c '$cmd'"
}

function install_command {
  run_command '
        if [ ! -e /aircloak/air/install/.installed ]; then
          until docker pull $REGISTRY_URL/aircloak/air-installer; do sleep 1; done &&
          installer_id=$(docker create $REGISTRY_URL/aircloak/air-installer) &&
          docker cp $installer_id:aircloak - > /tmp/aircloak.tar &&
          docker stop $installer_id &&
          docker rm -v $installer_id &&
          cd /tmp/ && tar -xf aircloak.tar -C / && rm aircloak.tar &&
          /aircloak/air/install/install.sh &&
          touch /aircloak/air/install/.installed;
        else
          echo "air already installed";
        fi
      '
}

function air_key_command {
  run_command '
        while
          [ ! -e /aircloak/ca/acinfra.aircloak.com.pem ] ||
          [ ! -e /aircloak/ca/aircloak.com.chain.pem ] ||
          [ ! -e /aircloak/ca/api.cert ] ||
          [ ! -e /aircloak/ca/api.key ]; do
            echo "waiting for air keys" && sleep 5;
        done && echo "got air keys"
      '
}