#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

# This will build a dockerized version of the cloak.
#
# To reduce the final image size, we build in two steps:
# 1. First, we produce the "builder" container. Here, we'll setup the full
#    Elixir environment, copy source, and create the release.
# 2. Then, we briefly start the builder container, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.

# Build deps locally
SYSTEM_VERSION=$(cat cloak/VERSION) \
  build_aircloak_image cloak_builder cloak/builder.dockerfile cloak/.dockerignore-builder

# Start the instance of the builder image and copy the generated release back to the disk
cd $ROOT_DIR/cloak
mkdir -p artifacts/rel
rm -rf artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name cloak_builder):latest)
docker cp $builder_container_id:/aircloak/cloak/rel/cloak/releases/0.0.1/cloak.tar.gz artifacts/rel/
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd artifacts/rel && \
  tar -xzf cloak.tar.gz && \
  rm cloak.tar.gz

# Build the release image
cd $ROOT_DIR
SYSTEM_VERSION=$(cat cloak/VERSION) \
  build_aircloak_image cloak cloak/release.dockerfile cloak/.dockerignore-release
