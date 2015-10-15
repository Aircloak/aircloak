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

# Setup common environment
cat /etc/environment >> /aircloak/air/environment
echo "REGISTRY_URL=$REGISTRY_URL" >> /aircloak/air/environment
echo "AIR_HOST_NAME=$COREOS_PUBLIC_IPV4" >> /aircloak/air/environment

# Purge memory, because it seems to affect starting of Docker containers
sync && echo 3 > /proc/sys/vm/drop_caches
