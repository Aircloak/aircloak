#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)

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
      --name "${container_name//_/-}" \\
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
  log
  log "Upgrading $DEPLOYMENT_NAME"
  log "- upgrading cloak"

  RUNTIME_CONFIG_PATH="/opt/share/cloak_runtime_configs/$DEPLOYMENT_NAME/"
  unset DOCKER_ARGS
  (start_component $DEPLOYMENT_NAME "cloak") >> ./publish_logs.txt

  log "- upgrading air"

  RUNTIME_CONFIG_PATH="/opt/share/air_runtime_configs/$DEPLOYMENT_NAME/"
  DOCKER_ARGS="-p $AIR_HTTP_PORT:8080 -p $AIR_PSQL_PORT:8432"
  (start_component $DEPLOYMENT_NAME "air") >> ./publish_logs.txt

  log "Done $DEPLOYMENT_NAME"
}

version=$(cat ./VERSION)

log ""
log "Starting upgrade process of public systems to version $version."
log "Logs are written to ./publish_logs.txt"
log "Update started on $(date)"

for target in `find ./deploy_targets -type f`
do
  # We need to unset the public system flag here,
  # otherwise all systems after the first public system
  # become public systems.
  unset PUBLIC_SYSTEM

  . "$(dirname ${BASH_SOURCE[0]})/$target"

  if [ $PUBLIC_SYSTEM ]; then
    deploy
  fi
done

log ""
log "Update complete"
