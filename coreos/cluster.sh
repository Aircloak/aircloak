#!/bin/bash

# This is a helper script meant to be run on the "provisioning server". The script
# can be used to:
#
#   - create initial cloud configuration
#   - reconfigure the cluster (add/remove members)
#   - upgrade individual machines or the entire cluster
#
# See `README.md` for usage description.

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh
. ./cloud-config.sh

function generate_cloud_config {
  mkdir -p tmp

  MACHINES="$@" INITIAL_CLUSTER_STATE=${INITIAL_CLUSTER_STATE:-"new"} \
  cloud_config > tmp/cloud-config
}

function add_machine {
  cluster_machine=$1
  new_machine=$2

  current_cluster="$(etcd_cluster $cluster_machine)"
  etcd_peer_port=$(get_tcp_port prod etcd/peer)

  # add new machine to the cluster
  cluster_etcdctl $1 member add $new_machine "http://$new_machine:$etcd_peer_port" >&2

  # generate cloud-config for the new machine
  cluster_machines=$(printf "$current_cluster\n$new_machine\n" | paste -sd " " -)
  INITIAL_CLUSTER_STATE=existing generate_cloud_config $cluster_machines
}

function etcd_cluster {
  cluster_etcdctl $1 member list | \
  awk '{print $3}' | \
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
    ssh $machine_to_remove "sudo /aircloak/air/air_service_ctl.sh stop_system" || true

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
  echo "Machine $1 upgraded."
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
  generate_cloud_config)
    shift
    if [ "$REGISTRY_URL" == "" ] || [ $# -eq 0 ]; then
      echo
      echo "Usage:"
      echo
      echo '  REGISTRY_URL=registry_ip[:registry_port] \'
      echo "  $0 generate_cloud_config machine1_ip machine2_ip ..."
      echo
      exit 1
    fi

    generate_cloud_config $@
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
    printf "\nUsage:\n  $0 generate_cloud_config | add_machine | remove_machine | upgrade_machine | rolling_upgrade\n\n"
    ;;
esac