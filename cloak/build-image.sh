#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

# This will build a dockerized version of the cloak.
#
# To reduce the final image size, we build in a few steps:
#
# 1. We start the base Elixir image, and fetch and build the dependencies. The result
#    is stored in the ../docker_cache/cloak folder which is mounted.
# 2. Then we build the release image, where sources as well as deps
#    are copied, and the OTP release is built.
# 3. Finally, we briefly start the release image, fetch the release locally,
#    and create the release container. Here, we just copy the release, without
#    the need to install Erlang.
#
# This approach allows us to cache dependencies locally. Hence, during the build
# we fetch/build only the changed dependencies. The release itself is built in
# the image so we rely on the docker layers caching. If neither sources, nor deps
# have been changed, the existing image will be reused.

# Build images are built without the category suffix
PROD_IMAGE_CATEGORY="$IMAGE_CATEGORY"
export IMAGE_CATEGORY=""

common/docker/elixir/build-image.sh

# build deps
echo "Building dependencies"
mkdir -p docker_cache/cloak/deps
mkdir -p docker_cache/cloak/_build
docker run --rm -i \
  -v $(pwd)/common:/aircloak/common \
  -v $(pwd)/cloak:/aircloak/cloak \
  -v $(pwd)/docker_cache/cloak/deps:/aircloak/cloak/deps \
  -v $(pwd)/docker_cache/cloak/_build:/aircloak/cloak/_build \
  $(aircloak_image_name elixir):latest \
  /bin/bash -c ". ~/.asdf/asdf.sh && cd /aircloak/cloak && MIX_ENV=prod ./fetch_deps.sh --only prod && MIX_ENV=prod mix compile"

# build the release
echo "Building the release"
build_aircloak_image \
  cloak_release_builder \
  cloak/docker/release-builder.dockerfile \
  cloak/docker/.dockerignore-release-builder

# Start the instance of the release image and copy the release back to the disk
echo "Building the release image"
cd $ROOT_DIR/cloak
mkdir -p artifacts/rel
rm -rf artifacts/rel/*
builder_container_id=$(docker create $(aircloak_image_name cloak_release_builder):latest)
docker cp $builder_container_id:/aircloak/cloak/rel/cloak/releases/0.1.0/cloak.tar.gz artifacts/rel/
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd artifacts/rel && \
  tar -xzf cloak.tar.gz && \
  rm cloak.tar.gz

# Build the release image
export IMAGE_CATEGORY="$PROD_IMAGE_CATEGORY"
cd $ROOT_DIR
SYSTEM_VERSION=$(cat cloak/VERSION) \
  build_aircloak_image cloak cloak/docker/release.dockerfile cloak/docker/.dockerignore-release
