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

function create_local_balancer_nginx_config {
  echo "
worker_processes  1;
daemon off;
error_log /dev/stdout info;

events {
  worker_connections 1024;
}

stream {
  upstream air_router_https {"

  for machine_num in $(seq 1 $1); do
    # IP addresses are predetermined in the Vagrantfile
    ip_address="172.17.8.$((100 + $machine_num))"
    echo "    server $ip_address:8200;"
  done

  echo "  }

  upstream air_router_http {"

  for machine_num in $(seq 1 $1); do
    # IP addresses are predetermined in the Vagrantfile
    ip_address="172.17.8.$((100 + $machine_num))"
    echo "    server $ip_address:8201;"
  done

  echo "  }

  server {
    listen 8998;
    proxy_pass air_router_https;
  }

  server {
    listen 8999;
    proxy_pass air_router_http;
  }
}"
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
      export DB_SERVER_URL=\"$COREOS_HOST_IP\" AIRPUB_URL=\"$COREOS_HOST_IP:1080\" AIR_ENDPOINT=\"$COREOS_HOST_IP:8999\" &&
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

# start local nginx
create_local_balancer_nginx_config $machines_num > ./local_balancer.conf
echo "Starting local balancer. You can access the site via http://frontend.air-local:8999 (or 8998 for https)"
nginx -c "$(pwd)/local_balancer.conf"