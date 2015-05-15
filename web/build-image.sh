#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Docker release image build
# -------------------------------------------------------------------

log "Building release container of rails app"
docker build -t aircloak/air_web:latest .
