#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

cp -rp ../config/config.sh docker
cp -rp ../config/tcp_ports.json docker

# -------------------------------------------------------------------
# Docker release image build
# -------------------------------------------------------------------

log "Building the balancer image"
setup_env_init
docker build -t aircloak/air_balancer:latest .
push_to_registry air_balancer
