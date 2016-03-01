SSH_OPTS="
  -o PasswordAuthentication=no
  -o IdentitiesOnly=yes
  -o User=aircloak
  -i /mnt/nfs/cosstage/ssh/id_rsa
"

function etcd_settings {
  cat ../etcd/etcd_values_stage
}

function prepare_machine {
  # copy docker registry auth info to the proper place
  machine_ssh $1 "cp -rp /mnt/nfs/cosstage/.docker/config.json /aircloak/docker_config.json"
}

function cloud_config_addon {
cat <<EOF
coreos:
  units:
    - name: update-engine.service
      drop-ins:
        - name: 50-proxy.conf
          content: |
            [Service]
            Environment=HTTPS_PROXY=http://acmirror.mpi-sws.org:3128
            Environment=HTTP_PROXY=http://acmirror.mpi-sws.org:3128
EOF
}