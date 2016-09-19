#!/usr/bin/env bash

set -eo pipefail

function lock_command {
  printf "
    echo 'Acquiring lock for $1'
    if lockfile -1 -r 120 /tmp/$1; then
      trap '{ rm -f /tmp/$1; }' EXIT
    else
      echo 'Could not acquire lock for $1.'
      exit 1
    fi
  "
}

function image_name {
  IMAGE_CATEGORY="$IMAGE_CATEGORY" ./container.sh image_name
}


function build_image {
  ssh acdbuild.mpi-sws.org "
    set -eo pipefail

    $(lock_command air_build)

    echo 'Pulling the latest version'
    cd $BUILD_FOLDER
    git fetch
    git checkout $BRANCH
    git reset --hard origin/$BRANCH
    echo 'Building the image'
    CONTAINER_ENV=prod IMAGE_CATEGORY=$IMAGE_CATEGORY $BUILD_FOLDER/package.sh
  "
}

function start_air {
  full_image_name="quay.io/$(image_name):$1"

  ssh $TARGET_MACHINE "
    set -eo pipefail

    $(lock_command $AIR_NAME)

    docker pull $full_image_name

    echo 'Stopping air $AIR_NAME'
    docker stop $AIR_NAME || true
    docker rm $AIR_NAME || true

    echo 'Starting air $AIR_NAME'
    docker run -d \\
      --name $AIR_NAME \\
      -e AIR_NAME=$AIR_NAME \\
      -p $AIR_HTTP_PORT:8080 \\
      -v /opt/share/air_runtime_configs/$RUNTIME_CONFIG/:/runtime_config \\
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

BUILD_FOLDER="/aircloak/quay_deploy/aircloak/air/site"
BRANCH=$(git symbolic-ref --short HEAD)

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
      published_image_versions $(image_name)
    "
    ;;

  rollback)
    start_air $2
    ;;

  deploy)
    if [ "$BRANCH" != "master" ]; then
      echo "Warning: deploying from branch $BRANCH"
      read -p "Continue (y/N)? " -r
      if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
    fi

    build_image
    start_air latest
    ;;

  *)
    print_usage
    exit 1
    ;;
esac
