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

log "Building release container of rails app"
setup_env_init
docker build -t aircloak/air_web:latest .
