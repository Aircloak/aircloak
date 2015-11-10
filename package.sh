#!/usr/bin/env bash

# This is a package script meant to be run on the "build server". Its purpose is
# to create most recent images of our services, version them, and push them to the
# docker registry.
#
# Usage: REGISTRY_URL=registry_url ./package.sh
# where REGISTRY_URL is in format registry_ip:tcp_port

set -eo pipefail

cd $(dirname $0)
. common/docker_helper.sh

function build_and_push {
  $1/build-image.sh

  image_name=$($1/container.sh image_name)
  version="$(latest_version $image_name)"

  if [ "$version" == "" ]; then
    echo "Can't find local version for $image_name"
    exit 1
  fi

  registry_version=$(
        registry_v2_req $REGISTRY_URL $image_name/tags/list |
        jq --raw-output ".tags | select(. != null) | .[]" |
        sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
        head -n 1
      )

  if [ "$registry_version" == "$version" ]; then
    echo "$image_name:$version already exists in the registry."
  else
    echo "Pushing $image_name:$version to the registry"
    docker tag -f "$image_name:$version" "$REGISTRY_URL/$image_name:$version"
    docker push "$REGISTRY_URL/$image_name:$version"
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

  curl -s "$auth_header" "$protocol://$1/v2/$2"
}

build_and_push coreos
build_and_push router
build_and_push backend
build_and_push frontend

# Remove all local repo tags. We don't need those, since the image is tagged
# anyway, and this allows us proper local cleanup of older images.
repo_tags=$(docker images | grep "$REGISTRY_URL" | awk '{print $1":"$2}' || true)
if [ "$repo_tags" != "" ]; then
  docker rmi $repo_tags
fi
