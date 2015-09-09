#!/usr/bin/env bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/../common/docker_helper.sh

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# -------------------------------------------------------------------
# Docker release image build
# -------------------------------------------------------------------

build_aircloak_image air_frontend frontend
push_to_registry air_frontend
