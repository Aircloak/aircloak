#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

if [ "$BUILD_BASE" != "false" ]; then common/docker/phoenix/build-image.sh; fi

# This will build a dockerized version of the air site.
#
# To reduce the final image size, we build in two steps:
# 1. First, we produce the "builder" container. Here, we'll setup the full
#    Elixir environment, copy source, and create the release.
# 2. Then, we briefly start the builder container, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.

current_version=$(cat VERSION)

# Build deps locally
SYSTEM_VERSION=$current_version \
  build_aircloak_image air_build air/builder.dockerfile air/.dockerignore-builder

# Start the instance of the builder image and copy the generated release back to the disk
cd $ROOT_DIR/air
mkdir -p artifacts/rel
rm -rf artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name air_build):latest)
docker cp $builder_container_id:/aircloak/air/_build/prod/rel/air/releases/$current_version/air.tar.gz artifacts/rel/
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd artifacts/rel && \
  tar -xzf air.tar.gz && \
  rm air.tar.gz

# Build the release image
cd $ROOT_DIR
SYSTEM_VERSION=$current_version \
  build_aircloak_image air air/release.dockerfile air/.dockerignore-release
