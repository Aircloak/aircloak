#!/usr/bin/env bash

set -eo pipefail

container_id=$1
args=${*:2}

function log {
  msg=$1
  echo "[aircloak] $msg"
}


# -------------------------------------------------------------------
# Run command in image
# -------------------------------------------------------------------

if [ -z "$args" ]; then
  log "Launching container $container_id"
  docker run -p 8080:8080 -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log --rm -it $container_id
else
  log "Executing command \"$args\" in container $container_id"
  docker run -p 8080:8080 -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log --rm -it $container_id $args
fi
