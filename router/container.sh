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

if [ "$AIR_HOST_NAME" != "" ]; then
  docker_env="$docker_env -e AIR_HOST_NAME=$AIR_HOST_NAME"
fi

if [ "$AIR_ENV" = "prod" ]; then
  cert_folder="/aircloak/ca"
else
  cert_folder="$(pwd)/dev_cert"
fi

DOCKER_START_ARGS=" $docker_env \
  -v $cert_folder:/aircloak/ca \
  --net=host \
  "$REGISTRY_URL"aircloak/air_router:latest \
  /aircloak/router/docker/start.sh"

REMOTE_CONSOLE_COMMAND="/bin/bash"

case "$1" in
  maintenance_on)
    curl -XPUT -L http://127.0.0.1:$(get_tcp_port prod etcd/client)/v2/keys/maintenance/down -d value=true
    ;;

  maintenance_off)
    curl -L http://127.0.0.1:$(get_tcp_port prod etcd/client)/v2/keys/maintenance/down -XDELETE
    ;;

  *)
    CUSTOM_COMMANDS="
      maintenance_on - turns on the maintenance mode
      maintenance_off - turns off the maintenance mode
    " container_ctl air_router $@
    ;;

esac
