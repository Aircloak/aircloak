#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh
stop_named_container air_frontend

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Run command in image
# -------------------------------------------------------------------

docker run --rm -it \
  --name air_frontend -p 8080:8080 -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log \
  "$@" aircloak/air_frontend:latest
