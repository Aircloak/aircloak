#!/usr/bin/env bash

set -eo pipefail

. /etc/environment

function pull_docker_image {
  echo "$(hostname): pulling image $1..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry for some magical number of times. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  #
  # The output is suppressed, because it's very noisy. Notice how only in the last attempt
  # the stderr output is let through. This allows us to print the error if pulling failed.
  docker pull $REGISTRY_URL/$1 > /dev/null 2>&1 ||
  docker pull $REGISTRY_URL/$1 > /dev/null 2>&1 ||
  docker pull $REGISTRY_URL/$1 > /dev/null 2>&1 ||
  docker pull $REGISTRY_URL/$1 > /dev/null 2>&1 ||
  docker pull $REGISTRY_URL/$1 > /dev/null

  echo "$(hostname): pulled image $1"
}

pull_docker_image aircloak/air_router:latest

cat /etc/environment >> /aircloak/air/environment
echo "REGISTRY_URL=$REGISTRY_URL" >> /aircloak/air/environment
echo "AIR_HOST_NAME=$COREOS_PUBLIC_IPV4" >> /aircloak/air/environment

echo "air system installed"
