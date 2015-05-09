#!/usr/bin/env bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Docker release image build
# -------------------------------------------------------------------

log "Building release container of rails app"
docker build .