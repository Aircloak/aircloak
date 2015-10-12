#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh

function machine_exec {
  # Suppress stderr, since it's just a "connection closed" message from `vagrant ssh`
  vagrant ssh $1 -c "$2" 2>/dev/null
}

function air_routers {
  for machine_num in $(seq 1 $1); do
    echo "192.168.55.$((100 + $machine_num))"
  done
}

function upload {
  scp_options=`vagrant ssh-config | awk -v ORS=' ' '{print "-o " $1 "=" $2}'`
  scp ${scp_options} "$@" > /dev/null
}

function machine_available {
  port=$(get_tcp_port prod air_frontend/http)
  for machine in $(air_routers); do
    code=$(curl \
          -s -o /dev/null \
          --header 'Host: frontend.air-local' \
          --connect-timeout 2 \
          -w "%{http_code}" \
          "http://$machine:$port"
        )
    if [ "$code" == "302" ]; then
      echo "$machine available"
      return 0;
    fi
  done

  return 1
}


if [ -z $COREOS_HOST_IP ]; then
  echo "
COREOS_HOST_IP not set. Please run with:

  COREOS_HOST_IP=w.x.y.z $0

where w.x.y.z is the IP of your host on your local network.
"
exit 1
fi

# Create CoreOS machines
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

# Wait for machines to boot
echo "Waiting for at least one machine to fully initialize... (this may take a while)"
# Tail logs in background so we can see some progress
vagrant ssh air-01 -c "journalctl -f -u air_installer -u air_config -u air_keys -u air_fleet" &
until machine_available; do sleep 1; done

# Start the local balancer
echo "Starting the local balancer"

function cleanup_routers {
  rm ../balancer/config/routers
}
trap cleanup_routers EXIT

air_routers > ../balancer/config/routers
../balancer/container.sh console
