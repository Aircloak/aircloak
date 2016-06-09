#!/usr/bin/env bash

set -eo pipefail

# build from the top-level folder of the project
cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh
. air/config/config.sh

SYSTEM_VERSION=$(cat air/VERSION) build_aircloak_image air_router air/router
