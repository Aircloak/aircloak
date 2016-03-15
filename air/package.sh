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
. common/docker_helper.sh

# This function builds a docker image and pushes it if needed. The build machine
# keeps the last built image, which allows it to determine whether the image has
# been changed and whether we need to push the new version after the build.
#
# The versioning algorithm is as follows:
#   1. The last version number is determined by the registry
#   2. If the `./VERSION` points to a different major/minor, the image
#      is always pushed to the registry with the version MAJOR.MINOR.0
#   3. If the new image is different from the previously built image,
#      and MAJOR.MINOR versions are unchanged, the patch version is bumped
#      and the image is pushed to the registry.
#   4. Otherwise, there are no changes so nothing is pushed to the registry.
#
# Caveats:
#   - If the build server is reinstalled, or images are deleted locally, next
#     build will always bump the version.
#   - If the last locally built image is not the same as in registry, but is the
#     same as the new built version, we won't push anything. This situation is
#     not very likely, but it's a potential downside of the chosen approach. If
#     you need to force the new version to be pushed, you can delete the local
#     image.
function build_and_push {
  image_name=$1

  latest_registry_version=$(
        registry_v2_req $REGISTRY_URL $image_name/tags/list |
        jq --raw-output ".tags | select(. != null) | .[]" |
        sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
        head -n 1
      )

  # build new image
  last_built_image_id=$(find_images $image_name ^latest$)
  eval "$2"
  new_image_id=$(find_images $image_name ^latest$)

  # determine the next version
  major_minor=$(cat ./VERSION)
  if [[ $latest_registry_version != $major_minor* ]]; then
    # different major/minor version
    new_version="$major_minor.0"
  elif [ "$last_built_image_id" != "$new_image_id" ]; then
    # same major/minor, but the image id is changed (i.e. there were changes in the image)
    last_patch_version=$(echo "$latest_registry_version" | sed s/^$major_minor\.//)
    new_version="$major_minor.$(($last_patch_version + 1))"
  else
    echo "No changes for $image_name"
    new_version=""
  fi

  # push if needed
  if [ "$new_version" != "" ]; then
    echo "Pushing $image_name:$new_version to the registry"
    docker tag -f "$new_image_id" "$REGISTRY_URL/$image_name:$new_version"
    docker push "$REGISTRY_URL/$image_name:$new_version"
  fi
}

function registry_v2_req {
  protocol="http"
  if [ -e "$HOME/.docker/config.json" ]; then
    token=$(
          cat $HOME/.docker/config.json |
          jq --raw-output ".auths[\"https://$1/v2/\"] | select(. != null) | .auth"
        )
    if [ "$token" != "" ]; then
      protocol="https"
      auth_header="-H 'Authorization: Basic $token'"
    fi
  fi

  eval "curl -s $auth_header $protocol://$1/v2/$2"
}

function push_air_image {
  build_and_push $($1/container.sh image_name) "$1/build-image.sh"
}

function push_static_site_image {
  if [ ! -e "$(pwd)/../../static-website" ]; then
    echo "Static website repo not found in $(pwd)/../../static-website"
    exit 1
  fi

  curdir=$(pwd)
  if [ "AIR_ENV" == "prod" ]; then site_build_env="production"; fi

  build_and_push \
      "aircloak/static_website" \
      "cd ../../static-website && BUILD_ENV='$site_build_env' ./build.sh && cd $(pwd)"
}

function check_registry {
  response=$(registry_v2_req $REGISTRY_URL "")

  if [ "$response" != "{}" ]; then
    printf "\nCan't connect to registry ($REGISTRY_URL): $response\n\n"
    exit 1
  fi
}

if [ "$IMAGE_CATEGORY" == "" ]; then
  echo "Please specify some deploy environment through IMAGE_CATEGORY variable."
  exit 1
fi

check_registry

push_static_site_image
push_air_image coreos
push_air_image router
push_air_image backend
push_air_image frontend
push_air_image site

# Remove all local repo tags. We don't need those, since the image is tagged
# anyway, and this allows us proper local cleanup of older images.
repo_tags=$(docker images | grep "$REGISTRY_URL" | awk '{print $1":"$2}' || true)
if [ "$repo_tags" != "" ]; then
  docker rmi $repo_tags
fi
