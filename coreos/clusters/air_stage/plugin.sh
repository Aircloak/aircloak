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