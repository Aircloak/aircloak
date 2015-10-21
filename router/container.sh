#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

DOCKER_IMAGE="aircloak/air_router:latest"

if [ "$AIR_ENV" = "prod" ]; then
  cert_folder="/aircloak/ca"
else
  cert_folder="$(pwd)/dev_cert"
fi

DOCKER_START_ARGS="--net=host -v $cert_folder:/aircloak/ca"
if [ "$AIR_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"; fi

CONTAINER_NAME="air_router"
CONTAINER_ARGS="/aircloak/router/docker/start.sh"

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
    "

    container_ctl $@
    ;;

esac
