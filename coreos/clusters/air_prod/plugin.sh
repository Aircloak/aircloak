SSH_OPTS="
  -o PasswordAuthentication=no
  -o IdentitiesOnly=yes
  -o User=aircloak
  -i /mnt/nfs/cosprod/id_rsa
"

function etcd_settings {
cat <<EOF
  $(cat ../etcd/etcd_values_prod)

  # temporarily disable integration tests on prod
  etcd_set /settings/air_backend/integration_test false
EOF
}

function prepare_machine {
  # copy docker registry auth info to the proper place
  machine_ssh $1 "cp -rp /mnt/nfs/cosprod/.docker/config.json /aircloak/docker_config.json"
}
