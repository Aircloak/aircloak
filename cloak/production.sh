#!/usr/bin/env bash

set -eo pipefail


function build_image {
  ssh acdbuild.mpi-sws.org "
    {
      lockfile -r 0 /tmp/cloak_deploy || {
        echo 'Another deploy in progress! Try again later.'
        exit 1
      }
    } && (
      {
        echo 'Pulling the latest version' &&
        cd $BUILD_FOLDER &&
        git fetch &&
        git checkout $BRANCH &&
        git reset --hard origin/$BRANCH &&
        echo 'Building the image' &&
        CONTAINER_ENV=prod REGISTRY_URL=$REGISTRY IMAGE_CATEGORY=$IMAGE_CATEGORY $BUILD_FOLDER/package.sh &&
        rm -f /tmp/cloak_deploy
      } || rm -f /tmp/cloak_deploy
    )
  "
}

function start_cloak {
  full_image_name="$REGISTRY/aircloak/${IMAGE_CATEGORY}_cloak:$1"

  ssh $TARGET_MACHINE "
    docker pull $full_image_name &&
    echo 'Stopping cloak $CLOAK_NAME' &&
    ( docker stop $CLOAK_NAME || true) &&
    ( docker rm $CLOAK_NAME || true) &&
    (
      if [ \"\$(docker ps | grep epmd)\" == \"\" ]; then
        docker run -d --net=host --name epmd $full_image_name /aircloak/cloak/erts-7.2.1/bin/epmd
      fi
    ) &&
    echo 'Starting cloak $CLOAK_NAME' &&
    docker run -d --net=host \\
      --name $CLOAK_NAME \\
      -e CLOAK_NAME=$CLOAK_NAME \\
      -v /opt/share/cloak_runtime_configs/$RUNTIME_CONFIG/:/runtime_config \\
      $full_image_name
  "
}

function print_usage {
  echo
  echo "Usage:"
  echo "  $0 deploy_target/desired_configuration deploy"
  echo "  $0 deploy_target/desired_configuration versions"
  echo "  $0 deploy_target/desired_configuration rollback version"
  echo
}

REGISTRY="registry.aircloak.com"
BUILD_FOLDER="/aircloak/cloak_mpi/aircloak/cloak/"
BRANCH=$(git symbolic-ref --short HEAD)
IMAGE="${IMAGE_CATEGORY}_cloak"

if [ $# -lt 2 ]; then
  print_usage
  exit 1
fi

. $1
shift

case "$1" in
  versions)
    ssh acdbuild.mpi-sws.org "
      . $BUILD_FOLDER/../docker/docker_helper.sh &&
      published_image_versions $REGISTRY aircloak/${IMAGE_CATEGORY}_cloak
    "
    ;;

  rollback)
    start_cloak $2
    ;;

  deploy)
    if [ "$BRANCH" != "master" ]; then
      echo "Warning: deploying from branch $BRANCH"
      read -p "Continue (y/N)? " -r
      if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
    fi

    build_image
    start_cloak latest
    ;;

  *)
    print_usage
    exit 1
    ;;
esac
