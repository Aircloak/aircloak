#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh

function machine_exec {
  # Suppress stderr, since it's just a "connection closed" message from `vagrant ssh`
  vagrant ssh $1 -c "$2" 2>/dev/null
}

function cluster_exec {
  # Useful for commands that have effects in the entire cluster.
  machine_exec air-01 "$1"
}

function destroy_service {
  cluster_exec "fleetctl stop $1 || true; fleetctl destroy $1 || true"
}

function air_routers {
  for machine_num in $(seq 1 $1); do
    printf "192.168.55.$((100 + $machine_num));"
  done
}

function upload {
  scp_options=`vagrant ssh-config | awk -v ORS=' ' '{print "-o " $1 "=" $2}'`
  scp ${scp_options} "$@" > /dev/null
}

if [ -z $COREOS_HOST_IP ]; then
  echo "
COREOS_HOST_IP not set. Please run with:

  COREOS_HOST_IP=w.x.y.z $0

where w.x.y.z is the IP of your host on your local network.
"
exit 1
fi

export INITIAL_CLUSTER_SIZE=${INITIAL_CLUSTER_SIZE:-1}

REGISTRY_URL="$COREOS_HOST_IP:$(get_tcp_port prod registry/http)" \
DB_SERVER_URL=$COREOS_HOST_IP \
./create_user_data.sh

vagrant destroy --force || true
vagrant up

# Upload keys
echo "Uploading keys"
for machine in $(vagrant status | grep 'air-' | grep running | awk '{print $1}'); do
  machine_exec $machine "sudo mkdir -p /aircloak/ca && sudo chown core:core /aircloak/ca"
  upload ../router/dev_cert/* $machine:/aircloak/ca
done
