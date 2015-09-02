#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

REGISTRY_URL=${REGISTRY_URL:-""}

if [ "$REGISTRY_URL" != "" ]; then
  REGISTRY_URL="$REGISTRY_URL""/"
fi

if [ "$AIR_ROUTERS" != "" ]; then
  DOCKER_START_ARGS="-e AIR_ROUTERS=$AIR_ROUTERS"
fi

if [ "$AIR_ENV" = "prod" ]; then
  https_port="443"
  http_port="80"
else
  https_port="8300"
  http_port="8301"
fi

DOCKER_START_ARGS="$DOCKER_START_ARGS -p $https_port:8300 -p $http_port:8301 \
  $docker_env \
  "$REGISTRY_URL"aircloak/air_balancer:latest \
  /aircloak/balancer/start.sh"

REMOTE_CONSOLE_COMMAND="/bin/bash"

container_ctl air_balancer $@