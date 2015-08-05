#!/bin/bash

set -eo pipefail

function setup_folder_structure {
  sudo mkdir -p /aircloak/air
  sudo mkdir -p /aircloak/air/frontend/log
  sudo chmod 777 /aircloak/air/frontend/log
  sudo chown core:core /aircloak/air

  rsync -arp /tmp/shared/air /aircloak/

  # Create start/stop scripts
  env="REGISTRY_URL=$DOCKER_REGISTRY_URL ETCD_PORT=4001"
  echo "$env /aircloak/air/backend/container.sh foreground" > /aircloak/air/backend_start.sh
  chmod +x /aircloak/air/backend_start.sh

  echo "$env /aircloak/air/backend/container.sh stop" > /aircloak/air/backend_stop.sh
  chmod +x /aircloak/air/backend_stop.sh

  env="REGISTRY_URL=$DOCKER_REGISTRY_URL ETCD_PORT=4001"
  echo "$env /aircloak/air/frontend/container.sh foreground" > /aircloak/air/frontend_start.sh
  chmod +x /aircloak/air/frontend_start.sh

  echo "$env /aircloak/air/frontend/container.sh stop" > /aircloak/air/frontend_stop.sh
  chmod +x /aircloak/air/frontend_stop.sh
}

function pull_docker_image {
  echo "Pulling image $1..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry for some magical number of times. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  #
  # The output is suppressed, because it's very noisy. Notice how only in the last attempt
  # the stderr output is let through. This allows us to print the error if pulling failed.
  docker pull $1 > /dev/null 2>&1 ||
  docker pull $1 > /dev/null 2>&1 ||
  docker pull $1 > /dev/null 2>&1 ||
  docker pull $1 > /dev/null 2>&1 ||
  docker pull $1 > /dev/null
}

function pull_images {
  echo "Pulling docker images, this may take a while..."
  pull_docker_image $DOCKER_REGISTRY_URL/aircloak/air_backend:latest
  pull_docker_image $DOCKER_REGISTRY_URL/aircloak/air_frontend:latest
}

setup_folder_structure
pull_images
/aircloak/air/etcd/config_coreos.sh
/aircloak/air/start_system.sh
