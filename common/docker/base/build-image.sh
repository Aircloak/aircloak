#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../../.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

# build the base image
build_aircloak_image \
  base \
  common/docker/base/Dockerfile \
  common/docker/base/.dockerignore \
  $(debian_version)
