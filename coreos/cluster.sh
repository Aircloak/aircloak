#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh
. ./cloud-config.sh

function init_cluster {
  # Check that prerequisites are met
  for required_file in $(echo "acinfra.aircloak.com.pem aircloak.com.chain.pem api.cert api.key"); do
    if [ ! -e "./ca/$required_file" ]; then
      echo "missing required local file $pwd/ca/$required_file"
      exit 1
    fi
  done

  echo "Checking machines for prerequisites..."
  for ip in "$@"; do check_machine $ip; done

  # generate cloud-config
  machines=$(
        for ip in "$@"; do
          echo "$ip,$(ssh $ip "cat /etc/machine-id")"
        done
      )
  mkdir -p tmp
  MACHINES="$machines" cloud_config > tmp/cloud-config

  # setup machines in parallel
  for ip in "$@"; do
    setup_machine $ip &
  done

  wait

  # At this point, all machines are installed and we should have the etcd cluster, so we can
  # do all subsequent actions on the first machine only, and it should affect the entire cluster.

  configure_etcd $1

  # submit services
  ssh $1 "fleetctl submit \
        /aircloak/air/air-backend@.service \
        /aircloak/air/air-frontend@.service \
        /aircloak/air/air-frontend-discovery@.service \
        /aircloak/air/air-router@.service
      "

  # start all service instances (as many as there are machines in the cluster)
  num_machines=$(echo "$@" | wc -w)
  service_indices="$(seq 1 $num_machines | paste -sd "," -)"
  if [ "$num_machines" -gt 1 ]; then service_indices="{$service_indices}"; fi
  ssh $1 "fleetctl start \
        air-backend@$service_indices \
        air-frontend@$service_indices \
        air-frontend-discovery@$service_indices \
        air-router@$service_indices
      "
}

function check_machine {
  ssh $1 "pwd" > /dev/null || {
    echo "Can't ssh to $1"
    exit 1
  }
  check $1 'whoami' "core" 'Invalid user, default SSH session must run as the `core` user'
  check $1 'sudo whoami' "root" 'No sudo permissions'
  check $1 '. /etc/os-release && echo "$NAME $VERSION"' "CoreOS 833.0.0" "Wrong CoreOS version, 833.0.0 required"
  check $1 '. /etc/environment && echo "$COREOS_PUBLIC_IPV4"' "$1" "Invalid public CoreOS IP address, expected $1"
  check $1 ' if [ ! -e /aircloak ]; then echo ok; fi' "ok" "Air system is already installed."

  echo "$1 ok"
}

function check {
  if [ "$(ssh $1 "$2")" != "$3" ]; then
    log_machine $1 "$4"
    exit 1
  fi
}

function setup_machine {
  start_installation $1
  upload_keys $1
  wait_for_install $1
}

function start_installation {
  scp tmp/cloud-config $1:/tmp/
  ssh $1 "
    sudo sh -c '
      mkdir -p /var/lib/coreos-install &&
      mv /tmp/cloud-config /var/lib/coreos-install/user_data &&
      coreos-cloudinit --from-file=/var/lib/coreos-install/user_data &
    '
  "
}

function upload_keys {
  log_machine $1 "uploading keys"
  ssh $1 "sudo mkdir -p /aircloak/ca && sudo chown core:core /aircloak/ca"
  scp ./ca/* $1:/aircloak/ca
}

function wait_for_install {
  log_machine $1 "waiting for the machine installation to finish (this may take a while)..."
  until machine_installed $1; do sleep 1; done
}

function machine_installed {
  result=$(ssh $1 'if [ -e /aircloak/air/install/.installed ]; then printf yes; else printf no; fi')
  if [ "$result" == "yes" ]; then return 0; else return 1; fi
}

function configure_etcd {
  log_machine $1 "waiting for etcd"
  until etcd_active $1; do sleep 1; done

  log_machine $1 "configuring etcd"
  ssh $1 "REGISTRY_URL='$REGISTRY_URL' DB_SERVER_URL='$DB_SERVER_URL' /aircloak/air/etcd/config_coreos.sh"
}

function etcd_active {
  result=$(ssh $1 'if [ $(systemctl is-active etcd2) == "active" ]; then printf yes; else printf no; fi')
  if [ "$result" == "yes" ]; then return 0; else return 1; fi
}

function log_machine {
  machine=$1
  shift
  echo "$machine: $@"
}

function kill_background_jobs {
  for child in $(jobs -p); do kill -9 "$child" || true; done
}
trap kill_background_jobs EXIT


case "$1" in
  init)
    shift
    if [ "$REGISTRY_URL" == "" ] || [ "DB_SERVER_URL" == "" ] || [ $# -eq 0 ]; then
      echo
      echo "Usage:"
      echo
      echo '  REGISTRY_URL=registry_ip[:registry_port] \'
      echo '  DB_SERVER_URL=db_server_ip \'
      echo "  $0 init machine1_ip machine2_ip ..."
      echo
      exit 1
    fi

    init_cluster $@
    ;;

  *)
    printf "\nUsage:\n  $0 init\n\n"
    ;;
esac