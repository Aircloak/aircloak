#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../../docker/docker_helper.sh

log "Starting etcd_air_dev"

init_env dev
DOCKER_IMAGE="quay.io/coreos/etcd"
DOCKER_IMAGE_VERSION="v2.0.6"
DOCKER_START_ARGS="--net=host"
CONTAINER_NAME="etcd_air_dev"
CONTAINER_ARGS=$(etcd_container_args)
container_ctl start

wait_for_etcd

log "Creating required ETCD values for development"
. etcd_values_dev
if [ -f local_settings/dev ]; then
  . ./local_settings/dev
fi
