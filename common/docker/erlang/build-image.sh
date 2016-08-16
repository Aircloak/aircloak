#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../../.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

common/docker/base/build-image.sh

# build the base image
build_aircloak_image \
  erlang \
  common/docker/erlang/Dockerfile \
  common/docker/erlang/.dockerignore \
  $(erlang_version)
