#!/bin/bash

set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
. $ROOT_DIR/docker/docker_helper.sh

function system_version {
  cat $ROOT_DIR/VERSION
}

function build_builder_image {
  cd $ROOT_DIR

  if [ "$BUILD_BASE" != "false" ]; then PREVENT_OLD_IMAGE_REMOVAL=true common/docker/phoenix/build-image.sh; fi

  SYSTEM_VERSION=$(system_version) \
    PREVENT_OLD_IMAGE_REMOVAL=true build_aircloak_image air_build air/builder.dockerfile air/.dockerignore-builder
}

function build_release {
  cd $ROOT_DIR

  # need to touch these files to ensure that they are mounted as files
  mkdir -p $(pwd)/$(air_cache_folder)/priv
  mkdir -p $(pwd)/$(air_cache_folder)/docs/content/

  touch -a $(pwd)/$(air_cache_folder)/priv/dependencies.zip
  touch -a $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.epub
  touch -a $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.pdf
  touch -a $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.mobi

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
    -v $(pwd)/$(air_cache_folder)/priv/dependencies.zip:/aircloak/air/priv/dependencies.zip \
    -v $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.epub:/aircloak/air/docs/content/aircloak-docs.epub \
    -v $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.pdf:/aircloak/air/docs/content/aircloak-docs.pdf \
    -v $(pwd)/$(air_cache_folder)/docs/content/aircloak-docs.mobi:/aircloak/air/docs/content/aircloak-docs.mobi \
    -v $(pwd)/$(air_cache_folder)/.gitbook:/root/.gitbook \
    -v $(pwd)/$(air_cache_folder)/cloak/deps:/aircloak/cloak/deps \
    -v $(pwd)/$(air_cache_folder)/bom/deps:/aircloak/bom/deps \
    -v $(pwd)/$(air_cache_folder)/bom/_build:/aircloak/bom/_build \
    $(aircloak_image_name air_build):$(git_head_image_tag) \
    /bin/bash -c '
      set -eo pipefail

      . ~/.asdf/asdf.sh
      . ~/.bashrc

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
}

function build_release_image {
  # copy the generated release to the artifacts folder
  cd $ROOT_DIR
  mkdir -p air/artifacts/rel
  rm -rf air/artifacts/rel/*
  cp -rp ./$(air_cache_folder)/_build/prod/rel/air/releases/$(system_version)/air.tar.gz air/artifacts/rel
  cd air/artifacts/rel
  tar -xzf air.tar.gz
  rm air.tar.gz

  # build the release image
  cd $ROOT_DIR
  SYSTEM_VERSION=$(system_version) \
    PREVENT_OLD_IMAGE_REMOVAL=true build_aircloak_image air air/release.dockerfile air/.dockerignore-release

  remove_old_git_head_image_tags "aircloak" > /dev/null 2>&1
}

function build_all {
  build_builder_image
  build_release
  build_release_image
}

case ${1:-all} in
	all)
    build_all
    ;;

  builder_image)
    build_builder_image
    ;;

  release)
    build_release
    ;;

  release_image)
    build_release_image
    ;;

  *)
    echo $0": unrecognized option:" $1
    exit 1
	  ;;
esac
