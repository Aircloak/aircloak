#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh

cd ../..

mkdir -p cloak/dev_container/cache

if [ ! -f cloak/dev_container/cache/.bash_history ]; then
  touch cloak/dev_container/cache/.bash_history
fi

CONTAINER_NAME="cloak_dev"
DOCKER_IMAGE="aircloak/cloak_dev"
DOCKER_START_ARGS="
  -v $(pwd):/aircloak \
  -v $(pwd)/cloak/dev_container/cache/.bash_history:/root/.bash_history
"

container_ctl $@
