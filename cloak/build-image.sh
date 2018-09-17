#!/bin/bash
set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
. $ROOT_DIR/docker/docker_helper.sh

function system_version {
  cat $ROOT_DIR/VERSION
}

function build_builder_image {
  cd $ROOT_DIR
  if [ "$BUILD_BASE" != "false" ]; then PREVENT_OLD_IMAGE_REMOVAL=true common/docker/rust/build-image.sh; fi
}

function build_release {
  cd $ROOT_DIR/cloak
  make odbc_drivers
  cd $ROOT_DIR

  # build the release
  docker run --rm -i \
    -v $(pwd)/VERSION:/aircloak/VERSION \
    -v $(pwd)/common:/aircloak/common \
    -v $(pwd)/cloak:/aircloak/cloak \
    -v $(pwd)/$(cloak_cache_folder)/deps:/aircloak/cloak/deps \
    -v $(pwd)/$(cloak_cache_folder)/_build:/aircloak/cloak/_build \
    -v $(pwd)/$(cloak_cache_folder)/.cargo:/root/.cargo \
    -v $(pwd)/$(cloak_cache_folder)/priv/native:/aircloak/cloak/priv/native \
    aircloak/rust:$(git_head_image_tag) \
    /bin/bash -c '
      set -eo pipefail
      . ~/.asdf/asdf.sh
      cd /aircloak/cloak
      MIX_ENV=prod ./fetch_deps.sh --only prod
      make release
    '
}

function build_release_image {
  # copy the generated release to the artifacts folder
  cd $ROOT_DIR
  mkdir -p cloak/artifacts/rel
  rm -rf cloak/artifacts/rel/*
  cp -rp ./$(cloak_cache_folder)/_build/prod/rel/cloak/releases/$(system_version)/cloak.tar.gz cloak/artifacts/rel
  cd cloak/artifacts/rel
  tar -xzf cloak.tar.gz
  rm cloak.tar.gz

  # build the release image
  cd $ROOT_DIR
  SYSTEM_VERSION=$(system_version) PREVENT_OLD_IMAGE_REMOVAL=true \
    build_aircloak_image cloak cloak/docker/release.dockerfile cloak/docker/.dockerignore-release

  remove_old_git_head_image_tags "aircloak"
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
