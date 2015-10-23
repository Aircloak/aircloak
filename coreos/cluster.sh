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
          echo "$ip,$(machine_id $ip)"
        done
      )
  mkdir -p tmp
  MACHINES="$machines" cloud_config > tmp/cloud-config

  # setup machines in parallel
  for ip in "$@"; do
    setup_machine $ip &
  done

  wait
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

function machine_id {
  ssh $1 "cat /etc/machine-id"
}

function setup_machine {
  start_installation $1
  upload_secrets $1
  follow_installation $1
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

function upload_secrets {
  log_machine $1 "uploading secrets"

  ssh $1 "
    sudo mkdir -p /aircloak/etcd &&
    sudo chown core:core /aircloak/etcd &&
    sudo mkdir -p /aircloak/ca &&
    sudo chown core:core /aircloak/ca
  "
  scp coreos_etcd $1:/aircloak/etcd/coreos
  scp ./ca/* $1:/aircloak/ca
}

function follow_installation {
  ssh $1 "
        while [ ! -e /aircloak/air/.installation_started ]; do sleep 1; done &&
        /aircloak/air/air_service_ctl.sh follow_installation
      "
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

function add_machine {
  cluster_machine=$1
  new_machine=$2

  check_machine $new_machine

  current_cluster="$(etcd_cluster $cluster_machine)"

  etcd_peer_port=$(get_tcp_port prod etcd/peer)

  # add new machine to the cluster
  machine_id=$(machine_id $new_machine)
  cluster_etcdctl $1 member add $machine_id "http://$new_machine:$etcd_peer_port"

  # generate cloud-config
  mkdir -p tmp

  MACHINES="$(echo "$current_cluster" && echo "$new_machine,$machine_id")" \
  INITIAL_CLUSTER_STATE=existing \
  cloud_config > tmp/cloud-config

  # setup the new machine
  setup_machine $new_machine

  # start new instances of air services
  ssh $new_machine "/aircloak/air/air_service_ctl.sh start_system"
}

function etcd_cluster {
  cluster_etcdctl $1 member list | \
  awk '{print $3 "," $2}' | \
  sed "s#peerURLs=http://##; s#name=##; s#:[0-9]*##" | \
  sort |
  uniq
}

function remove_machine {
  cluster_machine=$1
  machine_to_remove=$2

  etcd_machine_id="$(etcd_machine_id $cluster_machine $machine_to_remove)"

  if [ "$etcd_machine_id" != "" ]; then
    # Attempt to gracefully stop local services (in the case of running machine)
    ssh $machine_to_remove "/aircloak/air/air_service_ctl.sh stop_system" || true

    # Remove the machine from the cluster.
    cluster_etcdctl $cluster_machine member remove $etcd_machine_id
  else
    echo "$machine_to_remove is not a member of the cluster on $cluster_machine"
    exit 1
  fi
}

function etcd_machine_id {
  cluster_etcdctl $1 "member list" | \
  awk '{print $3 " " $1}' | \
  sed 's#peerURLs=http://##; s#:[0-9]*##; s#:$##' | \
  awk "{if (\$1 == \"$2\") print \$2}"
}

function cluster_etcdctl {
  etcd_client_port=$(get_tcp_port prod etcd/client)
  cluster_machine="$1"
  shift
  ssh $cluster_machine "etcdctl --endpoint 'http://127.0.0.1:$etcd_client_port' $@"
}

function upgrade_machine {
  ssh $1 "sudo /aircloak/air/air_service_ctl.sh upgrade_system"

  echo "Waiting for services to start..."
  retry=0
  while ! check_system $1; do
    retry=$((retry + 1))
    if [ $retry -gt 10 ]; then
      echo "Machine $1 services are not running! Please check the machine status, and reinstall if needed."
      exit 1
    fi

    sleep 5
  done

  echo "Machine $1 upgraded."
}

function check_system {
  system_ok=$(ssh $1 "/aircloak/air/air_service_ctl.sh check_system")
  if [ "$system_ok" == "yes" ]; then return 0; else return 1; fi
}

function rolling_upgrade {
  # Get all machines in the cluster
  current_cluster=$(etcd_cluster $1 | sed "s/,.*$//" | sort | uniq)

  for machine in $current_cluster; do
    echo "Upgrading $machine ..."
    upgrade_machine $machine || {
      echo "Upgrade of $machine failed, skipping other machines."
      exit 1
    }
    echo "$machine upgraded."
  done
}


function kill_background_jobs {
  for child in $(jobs -p); do
    {
      kill -9 $child > /dev/null 2>&1 && wait $child 2>/dev/null
    } || true
  done
}
trap kill_background_jobs EXIT


case "$1" in
  init)
    shift
    if [ "$REGISTRY_URL" == "" ] || [ $# -eq 0 ]; then
      echo
      echo "Usage:"
      echo
      echo '  REGISTRY_URL=registry_ip[:registry_port] \'
      echo "  $0 init machine1_ip machine2_ip ..."
      echo
      exit 1
    fi

    init_cluster $@
    ;;

  add_machine)
      shift
      if [ "$REGISTRY_URL" == "" ] || [ $# -ne 2 ]; then
        echo
        echo "Usage:"
        echo
        echo '  REGISTRY_URL=registry_ip[:registry_port] \'
        echo "  $0 add_machine cluster_machine_ip new_machine_ip"
        echo
        exit 1
      fi

      add_machine $1 $2
    ;;

  remove_machine)
      shift
      if [ $# -ne 2 ]; then
        echo
        echo "Usage:"
        echo
        echo "  $0 remove_machine cluster_machine_ip machine_to_remove_ip"
        echo
        exit 1
      fi

      remove_machine $1 $2
    ;;

  upgrade_machine)
      shift
      if [ $# -ne 1 ]; then
        echo
        echo "Usage:"
        echo
        echo "  $0 upgrade_machine cluster_machine_ip"
        echo
        exit 1
      fi

      upgrade_machine $1
    ;;

  rolling_upgrade)
      shift
      if [ $# -ne 1 ]; then
        echo
        echo "Usage:"
        echo
        echo "  $0 upgrade_machine cluster_machine_ip ..."
        echo
        exit 1
      fi

      rolling_upgrade $@
    ;;

  *)
    printf "\nUsage:\n  $0 init | add_machine | remove_machine | upgrade_machine | rolling_upgrade\n\n"
    ;;
esac