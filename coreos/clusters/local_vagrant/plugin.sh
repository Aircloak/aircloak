SSH_OPTS="
  -o PasswordAuthentication=no
  -o IdentitiesOnly=yes
  -o User=core
  -i ~/.vagrant.d/insecure_private_key
"

function etcd_settings {
  cat ../etcd/etcd_values_dev | sed 's/set_tcp_ports dev/set_tcp_ports prod/'

  if [ -e ../etcd/local_settings/dev ]; then
    cat ../etcd/local_settings/dev
  fi
}

function cloud_config_addon {
  echo ''
}
