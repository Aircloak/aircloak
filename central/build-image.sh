#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

common/docker/nodejs/build-image.sh

# This will build a dockerized version of the central site.
#
# To reduce the final image size, we build in two steps:
# 1. First, we produce the "builder" container. Here, we'll setup the full
#    Elixir environment, copy source, and create the release.
# 2. Then, we briefly start the builder container, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.

# Build deps locally
SYSTEM_VERSION=$(cat central/VERSION) \
  build_aircloak_image central_build central/builder.dockerfile central/.dockerignore-builder

# Start the instance of the builder image and copy the generated release back to the disk
cd $ROOT_DIR/central
mkdir -p artifacts/rel
rm -rf artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name central_build):latest)
docker cp $builder_container_id:/aircloak/central/rel/central/releases/0.0.1/central.tar.gz artifacts/rel/
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd artifacts/rel && \
  tar -xzf central.tar.gz && \
  rm central.tar.gz

# Build the release image
cd $ROOT_DIR
SYSTEM_VERSION=$(cat central/VERSION) \
  build_aircloak_image central central/release.dockerfile central/.dockerignore-release
