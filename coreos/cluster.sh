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

function setup_cluster {
  activate_cluster_plugin $1

  for machine_ip in $2; do
    check=$(machine_ssh $machine_ip "sudo whoami")
    if [ "$check" != "root" ]; then
      echo "Can't ssh as root on $machine_ip"
      exit 1
    fi
  done

  cloud_config_install_part=$(cloud_config_install_part "$2")

  for machine_ip in $2; do
    install_machine $machine_ip "$cloud_config_install_part"&
  done
  wait
}

function machine_ssh {
  eval "ssh $SSH_OPTS $1 \"$2\""
}


function activate_cluster_plugin {
  export CLUSTER_TYPE=$1
  plugin_file="clusters/$1/plugin.sh"
  if [ ! -e "$plugin_file" ]; then
    echo "Missing $(pwd)/$plugin_file"
    exit 1
  fi

  . $plugin_file

  # form full ssh_opts
  SSH_OPTS="
    -o LogLevel=FATAL
    -o StrictHostKeyChecking=no
    -o UserKnownHostsFile=/dev/null
    $SSH_OPTS
  "

  # convert newlines to spaces in SSH_OPTS
  SSH_OPTS=$(tr '\n' ' ' < <(echo "$SSH_OPTS"))
}

function cloud_config_install_part {
  MACHINES="$@" INITIAL_CLUSTER_STATE=${INITIAL_CLUSTER_STATE:-"new"} cloud_config
}

function install_machine {
  mkdir -p tmp

  # cloud-config
  cloud_config_file="$(generate_cloud_config_file $1 "$2")"
  upload_to_machine $1 root $cloud_config_file /var/lib/coreos-install/user_data
  rm $cloud_config_file

  # etcd config
  etcd_file="$(generate_etcd_settings $1)"
  upload_to_machine $1 default $etcd_file /tmp/etcd_values_coreos
  rm $etcd_file

  # keys
  upload_to_machine $1 default clusters/$CLUSTER_TYPE/secrets/ca /aircloak/ca

  # store cluster type on the machine
  machine_ssh $1 "echo '$CLUSTER_TYPE' > /aircloak/cluster_type"

  # allow plugin to perform some custom actions on the machine
  if [ $(type -t prepare_machine) ]; then prepare_machine $1; fi

  # Note that we're running cloudinit twice. The first one will start
  # the installation which will generate new cloud-config. Then,
  # we start the installation for the second time to start the installed
  # services. We're not doing it from the installer script, because that would
  # cause an endless recursion (trying to apply cloud-config while applying cloud-config).
  machine_ssh $1 "
    sudo coreos-cloudinit --from-file=/var/lib/coreos-install/user_data &&
    echo '$1: Applying the new cloud-config...' &&
    sudo coreos-cloudinit --from-file=/var/lib/coreos-install/user_data &&
    echo '$1: Air system successfully installed!'
  " &

  machine_ssh $1 "
        while [ ! -e /aircloak/air/.installation_started ]; do sleep 1; done &&
        sudo /aircloak/air/air_service_ctl.sh follow_installation
      "

  echo "$1: Waiting for services to start ..."
  machine_ssh $1 "/aircloak/air/air_service_ctl.sh wait_until_system_is_up"
}

function generate_cloud_config_file {
  # Take base cloud-config from the target machine, and append our custom stuff
  full_content=$(
    cat <<EOF
$(machine_ssh $1 "sudo cat /var/lib/coreos-install/user_data")

$2
EOF
    )

  # generate unique name
  cloud_config_file="tmp/cloud_config_$1_$((`date +%s`*1000+`date +%-N`/1000000))_$RANDOM"

  # Cloud-config vars (e.g. $public_ipv4) are not available to all installations
  # (see https://coreos.com/os/docs/latest/cloud-config.html), so we replace them
  # with hardcoded ip of the machine.
  echo "$full_content" | \
      sed "s/\$public_ipv4/$1/; s/\$private_ipv4/$1/" \
      > $cloud_config_file

  echo "$cloud_config_file"
}

function generate_etcd_settings {
  etcd_file="tmp/etcd_$1_$((`date +%s`*1000+`date +%-N`/1000000))_$RANDOM"
  cat <<EOF > $etcd_file
$(etcd_settings)

$(if [ -e clusters/$CLUSTER_TYPE/secrets/etcd ]; then cat clusters/$CLUSTER_TYPE/secrets/etcd; fi)
EOF
  echo "$etcd_file"
}

function upload_to_machine {
  if [ "$2" == "default" ]; then
    owner=$(machine_ssh $1 "whoami")
  else
    owner=$2
  fi

  eval "scp -r $SSH_OPTS $3 $1:/tmp/"

  target_dir=$(dirname $4)
  machine_ssh $1 "
    sudo mkdir -p $(dirname $4) &&
    sudo mv /tmp/$(basename $3) $4 &&
    sudo chown $owner:$owner $target_dir &&
    sudo chown $owner:$owner $4
  "
}

function add_machine {
  cluster_machine=$1
  new_machine=$2

  activate_cluster_plugin "$(machine_ssh $cluster_machine "cat /aircloak/cluster_type")"

  current_cluster="$(etcd_cluster $cluster_machine)"
  etcd_peer_port=$(get_tcp_port prod etcd/peer)

  # add new machine to the cluster
  cluster_etcdctl $cluster_machine member add $new_machine "http://$new_machine:$etcd_peer_port" >&2

  # generate cloud-config for the new machine
  cluster_machines=$(printf "$current_cluster\n$new_machine\n" | paste -sd " " -)
  cloud_config_install_part=$(INITIAL_CLUSTER_STATE=existing cloud_config_install_part "$cluster_machines")

  # install the new machine
  install_machine $new_machine "$cloud_config_install_part"
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
    machine_ssh $machine_to_remove "sudo /aircloak/air/air_service_ctl.sh stop_system" || true

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
  machine_ssh $cluster_machine "etcdctl --endpoint 'http://127.0.0.1:$etcd_client_port' $@"
}

function upgrade_machine {
  machine_ssh $1 "sudo /aircloak/air/air_service_ctl.sh upgrade_system"
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
  setup_cluster)
    shift
    if [ "$REGISTRY_URL" == "" ] || [ $# -lt 2 ]; then
      echo
      echo "Usage:"
      echo
      echo '  REGISTRY_URL=registry_ip[:registry_port] \'
      echo "  $0 setup_cluster cluster_type machine1_ip machine2_ip ..."
      echo
      exit 1
    fi

    setup_cluster "$@"
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

      add_machine $1 $2 $3 $4
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
        echo "  $0 rolling_upgrade cluster_machine_ip"
        echo
        exit 1
      fi

      rolling_upgrade $@
    ;;

  *)
    printf "\nUsage:\n  $0 setup_cluster | add_machine | remove_machine | upgrade_machine | rolling_upgrade\n\n"
    ;;
esac