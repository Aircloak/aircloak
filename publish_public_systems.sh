#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)

if [ $# -ne 1 ]; then
  echo
  echo "Usage:"
  echo "  $0 version"
  echo
  exit 1
fi

version=$1

function start_component {
  name=$1
  container_type=$2

  full_image_name="quay.io/aircloak/$container_type:$version"
  container_name="${name}_${container_type}"

  ssh $TARGET_MACHINE "
    set -eo pipefail

    docker pull $full_image_name

    echo 'Stopping container $container_name'
    docker stop $container_name || true
    docker rm $container_name || true

    echo 'Starting container $container_name'
    docker run -d -t \\
      --name "$container_name" \\
      -v $RUNTIME_CONFIG_PATH:/runtime_config \\
      $DOCKER_ARGS \\
      $full_image_name
  "
}

function log {
  message="${1}"
  echo "$message"
  echo "$message" >> ./publish_logs.txt
}

function deploy {
  name=$1

  . "$(dirname ${BASH_SOURCE[0]})/deploy_targets/$1"

  log
  log "Upgrading $name"
  log "- upgrading cloak"

  RUNTIME_CONFIG_PATH="/opt/share/cloak_runtime_configs/$DEPLOYMENT_NAME/"
  unset DOCKER_ARGS
  (start_component $name "cloak") >> ./publish_logs.txt

  log "- upgrading air"

  RUNTIME_CONFIG_PATH="/opt/share/air_runtime_configs/$DEPLOYMENT_NAME/"
  DOCKER_ARGS="-p $AIR_HTTP_PORT:8080 -p $AIR_PSQL_PORT:8432"
  (start_component $name "air") >> ./publish_logs.txt

  log "Done $name"
}

log ""
log "# -------------------------------------------------------------------"
log "Starting upgrade process of public systems."
log "Logging to upgrade to ./publish_logs.txt"
log "Update started on $(date)"

deploy "felix"
deploy "cnil"
deploy "kidesign"
log "# -------------------------------------------------------------------"
