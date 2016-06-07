#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../../docker/docker_helper.sh

log "Starting etcd_air_test"

init_env test
DOCKER_IMAGE="quay.io/coreos/etcd"
DOCKER_IMAGE_VERSION="v2.0.6"
DOCKER_START_ARGS="--net=host"
CONTAINER_NAME="etcd_air_test"
CONTAINER_ARGS=$(etcd_container_args)
container_ctl start

wait_for_etcd

log "Creating required ETCD values for development"
. etcd_values_test
if [ -f local_settings/test ]; then
  . ./local_settings/test
fi
