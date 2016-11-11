#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/central_runtime_config/$DEPLOYMENT_NAME/"
DOCKER_ARGS="-p $CENTRAL_HTTP_PORT:7080"

run_production_command central central "${DEPLOYMENT_NAME}_central" $@
