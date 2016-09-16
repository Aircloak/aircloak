#!/usr/bin/env bash

# This is a package script meant to be run on the "build server". Its purpose is
# to create most recent images of our services, version them, and push them to the
# docker registry.
#
# Usage: IMAGE_CATEGORY=some_env ./package.sh
# where
#   IMAGE_CATEGORY is arbitrary string which will added as the prefix of the image name.
#   This allows us to maintain different images for different environments (stage, prod).

set -eo pipefail

cd $(dirname $0)
. ../docker/docker_helper.sh


if [ "$IMAGE_CATEGORY" == "" ]; then
  echo "Please specify some deploy environment through IMAGE_CATEGORY variable."
  exit 1
fi

image_name=$(./container.sh image_name)

check_registry
build_and_push_to_registry $image_name "./build-image.sh" $(cat ./VERSION)
untag_registry_tags "$image_name"
