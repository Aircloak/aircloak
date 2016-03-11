#!/bin/bash

set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)

. $ROOT_DIR/common/docker_helper.sh

# This will build a dockerized version of the air site.
#
# To reduce the final image size, we build in two steps:
# 1. First, we produce the "builder" container. Here, we'll setup the full
#    Elixir environment, copy source, and create the release.
# 2. Then, we briefly start the builder container, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.

# Build deps locally
cd $ROOT_DIR
build_aircloak_image air_insights_build site/builder.dockerfile site/.dockerignore-builder

# Start the instance of the builder image and copy the generated release back to the disk
cd $ROOT_DIR
mkdir -p site/artifacts/rel
rm -rf site/artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name air_insights_build):latest)
docker cp $builder_container_id:/aircloak/air/site/rel/air/releases/0.0.1/air.tar.gz site/artifacts/rel/
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd site/artifacts/rel && \
  tar -xzf air.tar.gz && \
  rm air.tar.gz && \
  rm releases/0.0.1/air.tar.gz && \
  cd $ROOT_DIR

# Build the release image
build_aircloak_image air_insights site/release.dockerfile site/.dockerignore-release
