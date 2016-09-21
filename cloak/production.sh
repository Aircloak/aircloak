#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/cloak_runtime_configs/$DEPLOYMENT_NAME/"
run_production_command cloak cloak "${DEPLOYMENT_NAME}_cloak" $@
