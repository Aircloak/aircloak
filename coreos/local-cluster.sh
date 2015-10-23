#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh

function start_local_cluster {
  trap kill_cluster EXIT

  ../db/container.sh ensure_started
  package_system

  num_machines=${1:-1}

  # generate setting for local air_db
  echo "etcd_set /settings/air/db/host $COREOS_HOST_IP" > ./coreos_etcd

  REGISTRY_URL=$COREOS_HOST_IP:$(get_tcp_port prod registry/http) \
  ./cluster.sh generate_cloud_config $(machine_ips $num_machines)

  start_machines $num_machines

  for machine_ip in $(machine_ips $num_machines); do
    follow_installation $machine_ip &
    upload_secrets $machine_ip &
  done
  wait

  for machine_ip in $(machine_ips $num_machines); do
    ssh $machine_ip "journalctl -f -u air-*" &
  done

  start_local_balancer $num_machines

  sleep 10
  printf "\nCluster started!\n"
  printf "You can access the site at https://frontend.air-local:$(get_tcp_port prod balancer/https)\n"
  printf "To stop the system press Ctrl-C (only once)\n\n"

  wait
}

function package_system {
  ../docker_registry/container.sh ensure_started
  REGISTRY_URL="127.0.0.1:$(get_tcp_port prod registry/http)" ../package.sh
}

function kill_cluster {
  printf "\n\ndestroying cluster...\n"
  vagrant destroy -f
  ../balancer/container.sh stop
  rm ../balancer/config/routers || true
  cleanup_background_processes
}

function cleanup_background_processes {
  for child in $(jobs -p); do
    {
      kill -9 $child > /dev/null 2>&1 && wait $child 2>/dev/null
    } || true
  done
}

function start_machines {
  vagrant destroy -f
  vagrant up $(vagrant_names $1)
}

function upload_secrets {
  echo "$1 uploading secrets"

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

function machine_ips {
  for machine_num in $(seq 1 $1); do machine_ip $machine_num; done | \
  paste -sd " " -
}

function vagrant_names {
  for machine_num in $(seq 1 $1); do echo "air-0$machine_num"; done | \
  paste -sd " " -
}

function start_local_balancer {
  echo "Starting the local balancer"

  machine_ips $1 | tr ' ' '\n' > ../balancer/config/routers
  ../balancer/build-image.sh
  ../balancer/container.sh start
  docker logs -f air_balancer &
}

function machine_ip {
  machine_num=$(echo $1 | sed s/air-\0//)
  echo "192.168.55.$((100 + $machine_num))"
}

function ssh_config {
  trap kill_cluster EXIT
  vagrant up

  all_machines=$(vagrant status | grep running | awk '{print $1}')
  for machine in $all_machines; do
    machine_ip="$(machine_ip $machine)"
    config="$config$(vagrant ssh-config $machine --host $machine_ip)\n\n"
  done

  vagrant destroy -f
  trap - EXIT

  printf "\n\nAdd following to your ~/.ssh/config:\n\n$config\n"
}

function add_machine {
  trap cleanup_background_processes EXIT

  all_machines=$(vagrant status)

  cluster_machine=$(echo "$all_machines" | grep "running" | awk '{print $1}' | head -n 1 || true)
  if [ "$cluster_machine" == "" ]; then
    echo "Error: cluster is not running!"
    exit 1
  fi

  new_machine=$(echo "$all_machines" | grep "not created" | awk '{print $1}' | head -n 1 || true)
  if [ "$new_machine" == "" ]; then
    echo "Error: all available machines are running!"
    exit 1
  fi

  vagrant up $new_machine
  cluster_machine_ip="$(machine_ip $cluster_machine)"
  new_machine_ip="$(machine_ip $new_machine)"

  # add machine to the cluster
  REGISTRY_URL=$COREOS_HOST_IP:$(get_tcp_port prod registry/http) ./cluster.sh add_machine $cluster_machine_ip $new_machine_ip

  # update balancer configuration
  echo "$new_machine_ip" >> ../balancer/config/routers

  # tail logs
  ssh "$new_machine_ip" "journalctl -f -u air-*"
}

function remove_machine {
  trap cleanup_background_processes EXIT

  all_machines=$(vagrant status)

  cluster_machine=$(echo "$all_machines" | grep "running" | awk '{print $1}' | head -n 1 || true)
  if [ "$cluster_machine" == "" ]; then
    echo "Error: cluster is not running!"
    exit 1
  fi

  machine_to_remove=$(echo "$all_machines" | grep "running" | awk '{print $1}' | tail -n 1 || true)
  if [ "$machine_to_remove" == "$cluster_machine" ]; then
    echo "Last machine in the cluster -> destroying the cluster"
    vagrant destroy -f
    return 0
  fi

  cluster_machine_ip="$(machine_ip $cluster_machine)"
  machine_to_remove_ip="$(machine_ip $machine_to_remove)"

  # remove the machine from the balancer first
  new_routers=$(cat ../balancer/config/routers | grep -v "$machine_to_remove_ip")
  echo "$new_routers" > ../balancer/config/routers

  # remove the machine from the cluster
  ./cluster.sh remove_machine $cluster_machine_ip $machine_to_remove_ip || true

  # stop the machine
  vagrant destroy -f $machine_to_remove
}

function rolling_upgrade {
  package_system
  all_machines=$(vagrant status)

  cluster_machine=$(echo "$all_machines" | grep "running" | awk '{print $1}' | head -n 1 || true)
  if [ "$cluster_machine" == "" ]; then
    echo "Error: cluster is not running!"
    exit 1
  fi

  ./cluster.sh rolling_upgrade $(machine_ip $cluster_machine)
}


case "$1" in
  start)
    if [ -z $COREOS_HOST_IP ]; then
      printf "\nUsage:\n  COREOS_HOST_IP=w.x.y.z $0 start [num_machines]\n\n"
      exit 1
    fi

    shift
    start_local_cluster $@
    ;;

  add_machine)
    if [ -z $COREOS_HOST_IP ]; then
      printf "\nUsage:\n  COREOS_HOST_IP=w.x.y.z $0 add_machine\n\n"
      exit 1
    fi
    add_machine
    ;;

  remove_machine)
    remove_machine
    ;;

  upgrade_machine)
    shift
    if [ $# -ne 1 ]; then
      printf "\nUsage:\n  $0 upgrade_machine machine_name\n\n"
      exit 1
    fi
    package_system
    ./cluster.sh upgrade_machine $(machine_ip $1) || true
    ;;

  rolling_upgrade)
    rolling_upgrade
    ;;

  ssh-config)
    ssh_config
    ;;

  *)
    printf "\nUsage:\n  $0 start | add_machine | remove_machine | upgrade_machine | rolling_upgrade | ssh-config\n\n"
    ;;
esac
