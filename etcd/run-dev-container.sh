#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh

init_env dev

# Start etcd for configuration management
log "Starting etcd_air_test"

DOCKER_START_ARGS=$(docker_start_args)
container_ctl etcd_air_dev start
wait_for_etcd

log "Creating required ETCD values for development"
. etcd_values_dev
if [ -f local_settings/dev ]; then
  . ./local_settings/dev
fi