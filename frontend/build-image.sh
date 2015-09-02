#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Docker release image build
# -------------------------------------------------------------------

log "Building the frontend image"
setup_env_init
docker build -t aircloak/air_frontend:latest .

if named_container_running air_docker_registry; then
  log "Pushing to local registry"
  docker tag -f aircloak/air_frontend:latest localhost:5000/aircloak/air_frontend:latest
  docker push localhost:5000/aircloak/air_frontend:latest
else
  echo "Warning: local registry is not running, image not pushed."
fi
