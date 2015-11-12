#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh
. ../config/config.sh

DOCKER_IMAGE="aircloak/air_installer"
CONTAINER_NAME="air_installer"

container_ctl $@
