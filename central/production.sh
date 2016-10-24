#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/central_runtime_config/"
DOCKER_ARGS="-p $CENTRAL_HTTP_PORT:7080"

run_production_command central central "central" $@
