#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)

cd $ROOT_DIR/cloak
make odbc_drivers

cd $ROOT_DIR

. docker/docker_helper.sh

current_version=$(cat VERSION)

# build the builder image
if [ "$BUILD_BASE" != "false" ]; then PREVENT_OLD_IMAGE_REMOVAL=true common/docker/rust/build-image.sh; fi

# build the release
docker run --rm -i \
  -v $(pwd)/VERSION:/aircloak/VERSION \
  -v $(pwd)/common:/aircloak/common \
  -v $(pwd)/cloak:/aircloak/cloak \
  -v $(pwd)/$(cloak_cache_folder)/deps:/aircloak/cloak/deps \
  -v $(pwd)/$(cloak_cache_folder)/_build:/aircloak/cloak/_build \
  -v $(pwd)/$(cloak_cache_folder)/.cargo:/root/.cargo \
  $(aircloak_image_name rust):$(git_head_image_tag) \
  /bin/bash -c '
    set -eo pipefail
    . ~/.asdf/asdf.sh
    cd /aircloak/cloak
    MIX_ENV=prod ./fetch_deps.sh --only prod
    make release
  '

# copy the generated release to the artifacts folder
mkdir -p cloak/artifacts/rel
rm -rf cloak/artifacts/rel/*
cp -rp ./$(cloak_cache_folder)/_build/prod/rel/cloak/releases/$current_version/cloak.tar.gz cloak/artifacts/rel
cd cloak/artifacts/rel
tar -xzf cloak.tar.gz
rm cloak.tar.gz

# build the release image
cd $ROOT_DIR
SYSTEM_VERSION=$current_version PREVENT_OLD_IMAGE_REMOVAL=true \
  build_aircloak_image cloak cloak/docker/release.dockerfile cloak/docker/.dockerignore-release

remove_old_git_head_image_tags "aircloak"
