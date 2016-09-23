#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/air_runtime_configs/$DEPLOYMENT_NAME/"
DOCKER_ARGS="-p $AIR_HTTP_PORT:8080"

run_production_command air air "${DEPLOYMENT_NAME}_air" $@
