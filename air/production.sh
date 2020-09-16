#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../docker/production_helper.sh

RUNTIME_CONFIG_PATH="/opt/share/air_runtime_configs/$DEPLOYMENT_NAME/"
DOCKER_ARGS="-p $AIR_HTTP_PORT:8080 -p $AIR_PSQL_PORT:8432 -e http_proxy=http://acmirror.mpi-sws.org:3128 -e https_proxy=http://acmirror.mpi-sws.org:3128"

run_production_command air "${DEPLOYMENT_NAME}_air" $@