#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

DOCKER_IMAGE="aircloak/static_website"
DOCKER_START_ARGS="-p 10000:10000"
CONTAINER_NAME="static_website"
CONTAINER_ARGS="/aircloak/run.sh"

container_ctl $@
