#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh

init_env test

# Start etcd for configuration management
log "Starting etcd_air_test"

DOCKER_START_ARGS=$(docker_start_args)
container_ctl etcd_air_test start
wait_for_etcd

log "Creating required ETCD values for development"
. etcd_values_test
if [ -f local_settings/test ]; then
  . ./local_settings/test
fi