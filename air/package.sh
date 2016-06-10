#!/usr/bin/env bash

# This is a package script meant to be run on the "build server". Its purpose is
# to create most recent images of our services, version them, and push them to the
# docker registry.
#
# Usage: REGISTRY_URL=registry_url IMAGE_CATEGORY=some_env ./package.sh
# where
#   REGISTRY_URL is in format registry_ip:tcp_port
#   IMAGE_CATEGORY is arbitrary string which will added as the prefix of the image name.
#   This allows us to maintain different images for different environments (stage, prod).

set -eo pipefail

cd $(dirname $0)
. ../docker/docker_helper.sh

function push_air_image {
  build_and_push_to_registry $($1/container.sh image_name) "$1/build-image.sh" $(cat ./VERSION)
}

function push_static_site_image {
  if [ ! -e "$(pwd)/../../static-website" ]; then
    echo "Static website repo not found in $(pwd)/../../static-website"
    exit 1
  fi

  curdir=$(pwd)
  if [ "CONTAINER_ENV" == "prod" ]; then site_build_env="production"; fi

  build_and_push_to_registry \
      "aircloak/static_website" \
      "cd ../../static-website && BUILD_ENV='$site_build_env' ./build.sh && cd $(pwd)" \
      "$(cat ./VERSION)"
}


if [ "$IMAGE_CATEGORY" == "" ]; then
  echo "Please specify some deploy environment through IMAGE_CATEGORY variable."
  exit 1
fi

check_registry

push_static_site_image
push_air_image coreos
push_air_image router
push_air_image site

# Remove all local repo tags. We don't need those, since the image is tagged
# anyway, and this allows us proper local cleanup of older images.
repo_tags=$(docker images | grep "$REGISTRY_URL" | awk '{print $1":"$2}' || true)
if [ "$repo_tags" != "" ]; then
  docker rmi $repo_tags
fi
