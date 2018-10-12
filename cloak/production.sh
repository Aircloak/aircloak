#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/cloak_runtime_configs/$DEPLOYMENT_NAME/"
PERIST_PATH="${RUNTIME_CONFIG_PATH}/persist"
DOCKER_ARGS="-v /opt/share/cloak_odbc_drivers/:/odbc_drivers -v ${PERIST_PATH}:/persist"
run_production_command cloak cloak "${DEPLOYMENT_NAME}_cloak" $@
