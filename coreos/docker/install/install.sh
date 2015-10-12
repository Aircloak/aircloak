#!/usr/bin/env bash

set -eo pipefail

. /etc/environment

function pull_docker_image {
  echo "$(hostname): pulling image $1..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry for some magical number of times. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  docker pull $REGISTRY_URL/$1 ||
  docker pull $REGISTRY_URL/$1 ||
  docker pull $REGISTRY_URL/$1 ||
  docker pull $REGISTRY_URL/$1 ||
  docker pull $REGISTRY_URL/$1

  echo "$(hostname): pulled image $1"
}

# Pull images
pull_docker_image aircloak/air_router:latest
pull_docker_image aircloak/air_backend:latest
pull_docker_image aircloak/air_frontend:latest

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
  /aircloak/air/router@.service \
  /aircloak/air/backend@.service \
  /aircloak/air/frontend@.service \
  /aircloak/air/frontend-discovery@.service

# Start units
fleetctl start \
  "router@$service_indices" \
  "backend@$service_indices" \
  "frontend@$service_indices" \
  "frontend-discovery@$service_indices"

echo "air system installed"
