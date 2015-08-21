#!/bin/bash

set -eo pipefail

function create_helper_scripts {
  echo "${2} /aircloak/air/${1}/container.sh foreground" > /aircloak/air/${1}_start.sh
  chmod +x /aircloak/air/${1}_start.sh

  echo "${2} /aircloak/air/${1}/container.sh stop" > /aircloak/air/${1}_stop.sh
  chmod +x /aircloak/air/${1}_stop.sh
}

function setup_folder_structure {
  sudo mkdir -p /aircloak/air
  sudo mkdir -p /aircloak/air/frontend/log
  sudo chmod 777 /aircloak/air/frontend/log
  sudo chown core:core /aircloak/air

  rsync -arp /tmp/shared/air /aircloak/

  # Create start/stop scripts
  create_helper_scripts backend \
    "REGISTRY_URL=$DOCKER_REGISTRY_URL ETCD_PORT=4001 AIR_HOST_NAME=$COREOS_PUBLIC_IPV4 EXPORT_BEAM_PORTS=true"

  create_helper_scripts frontend "REGISTRY_URL=$DOCKER_REGISTRY_URL ETCD_PORT=4001 AIR_HOST_NAME=$COREOS_PUBLIC_IPV4"
  create_helper_scripts router "REGISTRY_URL=$DOCKER_REGISTRY_URL ETCD_PORT=4001 AIR_HOST_NAME=$COREOS_PUBLIC_IPV4"
}

. /etc/environment
setup_folder_structure


cat <<EOF > /aircloak/air/pull_images.sh

function pull_docker_image {
  echo "\$(hostname): pulling image \$1..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry for some magical number of times. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  #
  # The output is suppressed, because it's very noisy. Notice how only in the last attempt
  # the stderr output is let through. This allows us to print the error if pulling failed.
  docker pull \$1 > /dev/null 2>&1 ||
  docker pull \$1 > /dev/null 2>&1 ||
  docker pull \$1 > /dev/null 2>&1 ||
  docker pull \$1 > /dev/null 2>&1 ||
  docker pull \$1 > /dev/null

  echo "\$(hostname): pulled image \$1"
}

pull_docker_image $DOCKER_REGISTRY_URL/aircloak/air_backend:latest
pull_docker_image $DOCKER_REGISTRY_URL/aircloak/air_frontend:latest
pull_docker_image $DOCKER_REGISTRY_URL/aircloak/air_router:latest
EOF

chmod +x /aircloak/air/pull_images.sh
