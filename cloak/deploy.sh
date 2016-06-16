#!/usr/bin/env bash

set -eo pipefail


function build_image {
  ssh acdbuild.mpi-sws.org "
    echo 'Pulling the latest version' &&
    cd $BUILD_FOLDER &&
    git fetch &&
    git checkout $BRANCH &&
    git reset --hard origin/$BRANCH &&
    echo 'Building the image' &&
    CONTAINER_ENV=prod REGISTRY_URL=$REGISTRY IMAGE_CATEGORY=$IMAGE_CATEGORY \\
      $BUILD_FOLDER/package.sh
  "
}

function upgrade_cloak {
  # Note that while starting, we're starting an epmd in a separate container. This allows us to
  # restart cloak containers without stopping epmd instance. On subsequent deploys, starting of
  # epmd will fail (since it's already running), but we'll just ignore that error and move on.
  ssh $TARGET_MACHINE "
    docker pull $IMAGE &&
    echo 'Stopping cloak $CLOAK_NAME' &&
    ( docker stop $CLOAK_NAME || true) &&
    ( docker rm $CLOAK_NAME || true) &&
    ( docker run -d --net=host --name epmd $IMAGE /aircloak/cloak/erts-7.2.1/bin/epmd || true) &&
    echo 'Starting cloak $CLOAK_NAME' &&
    docker run -d --net=host \\
      --name $CLOAK_NAME \\
      -e CLOAK_NAME=$CLOAK_NAME \\
      -v /opt/share/cloak_runtime_configs/$RUNTIME_CONFIG/:/runtime_config \\
      $IMAGE
  "
}


if [ "$#" == "4" ]; then
  IMAGE_CATEGORY=$1
  TARGET_MACHINE=$2
  RUNTIME_CONFIG=$3
  CLOAK_NAME=$4
elif [ "$#" == "1" ]; then
  . $1
else
  echo
  echo "Usage:"
  echo "  $0 deploy_target/desired_configuration"
  echo "  $0 image_category target_machine runtime_config cloak_name"
  echo
  exit 1
fi

REGISTRY="registry.aircloak.com"
BUILD_FOLDER="/aircloak/cloak_mpi/aircloak/cloak/"
BRANCH=$(git symbolic-ref --short HEAD)
IMAGE="$REGISTRY/aircloak/${IMAGE_CATEGORY}_cloak:latest"

if [ "$BRANCH" != "master" ]; then
  echo "Warning: deploying from branch $BRANCH"
  read -p "Continue (y/N)? " -r
  if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
fi

build_image
upgrade_cloak
