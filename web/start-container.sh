#!/usr/bin/env bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Run command in image
# -------------------------------------------------------------------

docker run -p 8080:8080 -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log --rm -it "$@" aircloak/air_web:latest
