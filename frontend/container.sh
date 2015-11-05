#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

DOCKER_IMAGE="aircloak/air_frontend"

if [ "$AIR_ENV" = "prod" ]; then
  cert_folder="/aircloak/ca"
else
  cert_folder="$(pwd)/../router/dev_cert"
fi

DOCKER_START_ARGS="--net=host -v $cert_folder:/aircloak/ca"
if [ "$AIR_HOST_NAME" != "" ]; then DOCKER_START_ARGS="$DOCKER_START_ARGS -e AIR_HOST_NAME=$AIR_HOST_NAME"; fi

CONTAINER_NAME="air_frontend"
CONTAINER_ARGS="/aircloak/website/docker/start.sh"
REMOTE_CONSOLE_COMMAND='. docker/config.sh && ETCD_CLIENT_PORT=$(get_tcp_port prod etcd/client) bundle exec rails c'

container_ctl $@
