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


if [ "$IMAGE_CATEGORY" == "" ]; then
  echo "Please specify some deploy environment through IMAGE_CATEGORY variable."
  exit 1
fi

check_registry

build_and_push_to_registry $(./container.sh image_name) "./build-image.sh" $(cat ./VERSION)

untag_registry_tags "$REGISTRY_URL"
