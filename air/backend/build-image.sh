#!/bin/bash

set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)

. $ROOT_DIR/common/docker_helper.sh

# This will build a dockerized version of the air-sandbox app.
# It has one major nasty caveat. Since we don't want to include
# the SSH keys required to get at the private github repositories
# inside the docker containers (or any of the file system layers),
# we need to fetch dependencies locally.
#
# Moreover, to reduce the final image size, we build in two steps:
# 1. First, we produce the "builder" container. Here, we'll setup the full
#    Erlang environment, copy source, and create the release.
# 2. Then, we briefly start the builder container, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.

# Build deps locally
cd $ROOT_DIR
mkdir -p backend/artifacts/cache
cp -rp backend/Makefile backend/artifacts/cache
cp -rp backend/rebar backend/artifacts/cache
cp -rp backend/rebar.config backend/artifacts/cache
cp -rp backend/rebar.config.lock backend/artifacts/cache
cd backend/artifacts/cache
make deps
cd deps/cloak && git submodule update --init --recursive

# Build the builder image.
build_aircloak_image air_backend_build backend/builder.dockerfile backend/.dockerignore-builder

# Start the instance of the builder image and copy the generated release back to the disk
cd $ROOT_DIR
mkdir -p backend/artifacts/rel
rm -rf backend/artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name air_backend_build):latest)
docker cp $builder_container_id:/tmp/web/backend/rel/air - > backend/artifacts/rel/air.tar
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd backend/artifacts/rel && tar -xf air.tar && rm air.tar && cd $ROOT_DIR

# Build the release image
build_aircloak_image air_backend backend/release.dockerfile backend/.dockerignore-release
