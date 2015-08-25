#!/bin/bash

set -eo pipefail

cd $(dirname $0)

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


if [ -z $COREOS_HOST_IP ]; then
  echo "
COREOS_HOST_IP not set. Please run with:

  COREOS_HOST_IP=w.x.y.z $0

where w.x.y.z is the IP of your host on your local network.
"
exit 1
fi

./create_user_data.sh

vagrant halt --force || true
vagrant up --provision

# Generate string in form of {1,2,...,n}. Such string can be used with various fleetctl commands to
# collectively start/stop particular instances.
machines_num=$(vagrant status | grep 'air-' | grep -c running)
service_indices="{$(seq 1 $machines_num | paste -sd "," -)}"

# pull images
echo "Pulling docker images, this may take a while..."
for machine in $(vagrant status | grep 'air-' | grep running | awk '{print $1}'); do
  machine_exec $machine "/aircloak/air/pull_images.sh" &
done
wait

# destroy all services
destroy_service "router@$service_indices frontend-discovery@$service_indices frontend@$service_indices backend@$service_indices"
cluster_exec "fleetctl destroy router@.service frontend-discovery@.service frontend@.service backend@.service"


# configure etcd
cluster_exec "
      export DB_SERVER_URL=\"$COREOS_HOST_IP\" AIRPUB_URL=\"$COREOS_HOST_IP:1080\" &&
      /aircloak/air/etcd/config_coreos.sh
    "

# start services
cluster_exec "fleetctl submit /aircloak/air/backend@.service /aircloak/air/frontend@.service /aircloak/air/frontend-discovery@.service /aircloak/air/router@.service"
cluster_exec "fleetctl start \
      backend@$service_indices \
      frontend@$service_indices \
      frontend-discovery@$service_indices \
      router@$service_indices
    "

# start the balancer
echo "
Starting the balancer.
You can access the site via https://frontend.air-local:8300
"

../balancer/build-image.sh
AIR_ROUTERS=$(air_routers $machines_num) ../balancer/container.sh console
