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
EOF

chmod +x /aircloak/air/pull_images.sh

cat <<EOF > /aircloak/air/start_system.sh
#!/bin/bash

set -eo pipefail

function start_air_service {
  fleetctl destroy \$1
  fleetctl load /aircloak/air/\$1
  fleetctl start \$1
}

export DB_SERVER_URL='$DB_SERVER_URL'
export AIRPUB_URL='$AIRPUB_URL'

/aircloak/air/etcd/config_coreos.sh

start_air_service backend.service
start_air_service backend-discovery.service

start_air_service frontend.service
start_air_service frontend-discovery.service

EOF

chmod +x /aircloak/air/start_system.sh