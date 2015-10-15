#!/usr/bin/env bash

set -eo pipefail

. /etc/environment

function pull_docker_image {
  echo "$(hostname): pulling image $1..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry until pulling succeeds. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  until docker pull $REGISTRY_URL/$1; do
    sleep 1
  done

  echo "$(hostname): pulled image $1"
}

# Pull images
pull_docker_image aircloak/air_router:latest
pull_docker_image aircloak/air_backend:latest
pull_docker_image aircloak/air_frontend:latest

# Purge memory, because it seems to affect starting of Docker containers
sync && echo 3 > /proc/sys/vm/drop_caches

# Setup common environment
cat /etc/environment >> /aircloak/air/environment
echo "REGISTRY_URL=$REGISTRY_URL" >> /aircloak/air/environment
echo "AIR_HOST_NAME=$COREOS_PUBLIC_IPV4" >> /aircloak/air/environment

# Compute machines suffix in form of {1,2,...} or just 1 (for a single machine cluster)
machines_num=$(($(fleetctl list-machines | wc -l) - 1))
service_indices="$(seq 1 $machines_num | paste -sd "," -)"
if [ $machines_num -gt 1 ]; then service_indices="{$service_indices}"; fi

# Submit unit files
fleetctl submit \
  /aircloak/air/air-router@.service \
  /aircloak/air/air-backend@.service \
  /aircloak/air/air-frontend@.service \
  /aircloak/air/air-frontend-discovery@.service

# Start units
fleetctl start \
  "air-router@$service_indices" \
  "air-backend@$service_indices" \
  "air-frontend@$service_indices" \
  "air-frontend-discovery@$service_indices"

echo "air system installed"
