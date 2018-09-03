#!/bin/bash

set -eo pipefail

# build from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. docker/docker_helper.sh

current_version=$(cat VERSION)

if [ "$BUILD_BASE" != "false" ]; then PREVENT_OLD_IMAGE_REMOVAL=true common/docker/phoenix/build-image.sh; fi

# build the builder image
SYSTEM_VERSION=$current_version \
  PREVENT_OLD_IMAGE_REMOVAL=true build_aircloak_image air_build air/builder.dockerfile air/.dockerignore-builder

# build the release
docker run --rm -i \
  -v $(pwd)/VERSION:/aircloak/VERSION \
  -v $(pwd)/common:/aircloak/common \
  -v $(pwd)/air:/aircloak/air \
  -v $(pwd)/bom:/aircloak/bom \
  -v $(pwd)/cloak:/aircloak/cloak \
  -v $(pwd)/$(air_cache_folder)/_build:/aircloak/air/_build \
  -v $(pwd)/$(air_cache_folder)/deps:/aircloak/air/deps \
  -v $(pwd)/$(air_cache_folder)/assets/node_modules:/aircloak/air/assets/node_modules \
  -v $(pwd)/$(air_cache_folder)/docs/node_modules:/aircloak/air/docs/node_modules \
  -v $(pwd)/$(air_cache_folder)/.gitbook:/root/.gitbook \
  -v $(pwd)/$(air_cache_folder)/cloak/deps:/aircloak/cloak/deps \
  -v $(pwd)/$(air_cache_folder)/bom/deps:/aircloak/bom/deps \
  -v $(pwd)/$(air_cache_folder)/bom/_build:/aircloak/bom/_build \
  $(aircloak_image_name air_build):$(git_head_image_tag) \
  /bin/bash -c '
    set -eo pipefail
    . ~/.asdf/asdf.sh

    cd /aircloak/air
    ./fetch_deps.sh --only prod

    cd assets
    yarn install
    cd ..

    cd docs
    yarn install
    gitbook_version=$(cat /aircloak/air/docs/node_modules/gitbook/package.json | jq -r ".version")
    if [ ! -d /root/.gitbook/versions/$gitbook_version ]; then yarn run gitbook fetch $gitbook_version; fi
    cd ..

    cd /aircloak/cloak
    ./fetch_deps.sh --only prod

    cd /aircloak/bom
    ./fetch_deps.sh

    mix bom \
      --elixir /aircloak/air/deps \
      --elixir /aircloak/cloak/deps \
      --rust /aircloak/cloak/src/rodbc \
      --node /aircloak/air/assets/node_modules \
      /aircloak/air/priv

    cd /aircloak/air
    make release
  '

# copy the generated release to the artifacts folder
mkdir -p air/artifacts/rel
rm -rf air/artifacts/rel/*
cp -rp ./$(air_cache_folder)/_build/prod/rel/air/releases/$current_version/air.tar.gz air/artifacts/rel
cd air/artifacts/rel
tar -xzf air.tar.gz
rm air.tar.gz

# build the release image
cd $ROOT_DIR
SYSTEM_VERSION=$current_version \
  PREVENT_OLD_IMAGE_REMOVAL=true build_aircloak_image air air/release.dockerfile air/.dockerignore-release

remove_old_git_head_image_tags "aircloak" > /dev/null 2>&1
