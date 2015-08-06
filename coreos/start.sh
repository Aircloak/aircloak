#!/bin/bash

set -eo pipefail

cd $(dirname $0)

if [ -z $COREOS_HOST_IP ]; then
  echo "
COREOS_HOST_IP not set. Please run with:

  COREOS_HOST_IP=x.y.z $0

where x.y.z is the IP of your host on your local network.
"
exit 1
fi

./create_user_data.sh

vagrant halt --force || true
vagrant up --provision

echo "Pulling docker images, this may take a while..."
for machine in $(vagrant status | grep 'air-' | grep running | awk '{print $1}'); do
  vagrant ssh $machine -c "/aircloak/air/pull_images.sh" &
done
wait

# Stop all services on all machines.
for machine in $(vagrant status | grep 'air-' | grep running | awk '{print $1}'); do
  vagrant ssh $machine -c "fleetctl stop backend.service || true"
  vagrant ssh $machine -c "fleetctl destroy backend.service || true"

  vagrant ssh $machine -c "fleetctl stop frontend.service || true"
  vagrant ssh $machine -c "fleetctl destroy frontend.service || true"

  vagrant ssh $machine -c "fleetctl stop frontend-discovery.service || true"
  vagrant ssh $machine -c "fleetctl destroy frontend-discovery.service || true"
done

# We need to start the system on a single machine only. This will cause the services to be started in the
# entire cluster.
vagrant ssh air-01 -c "/aircloak/air/start_system.sh"
