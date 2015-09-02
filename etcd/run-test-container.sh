#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ./etcd_lib.sh
. ../common/docker_helper.sh


# -------------------------------------------------------------------
# Setup
# -------------------------------------------------------------------

if [ -n "$(env | grep boot2docker)" ]; then
  log "Assuming using boot2docker due to environment variables"
  ETCD_DEFAULT_IP=$(boot2docker ip)
else
  ETCD_DEFAULT_IP="127.0.0.1"
fi

export ETCD_PORT=4004
export HOST_IP=${ETCD_HOST_IP:-$ETCD_DEFAULT_IP}
export ETCD=$HOST_IP:$ETCD_PORT


# -------------------------------------------------------------------
# etcd
# -------------------------------------------------------------------

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