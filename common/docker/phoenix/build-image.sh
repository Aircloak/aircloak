#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../../.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

common/docker/elixir/build-image.sh

# build the base image
IMAGE_CATEGORY="" build_aircloak_image \
  phoenix \
  common/docker/phoenix/Dockerfile \
  common/docker/phoenix/.dockerignore \
  $(nodejs_version)
