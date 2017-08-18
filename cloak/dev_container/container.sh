#!/bin/bash

set -e

cd $(dirname $0)
. ../../docker/docker_helper.sh

cd ../..

mkdir -p cloak/dev_container/cache

if [ ! -f cloak/dev_container/cache/.bash_history ]; then
  touch cloak/dev_container/cache/.bash_history
fi

mounted_from_root="VERSION common/elixir"
mounted_from_cloak="config datagen include lib perftest priv rel test mix.exs mix.lock Makefile"
mounted_from_cloak_cache="deps _build .bash_history"

mounts="-v $(pwd)/cloak/dev_container/cache/.bash_history:/root/.bash_history"

for path in $mounted_from_root; do
  mounts="$mounts -v $(pwd)/$path:/aircloak/$path"
done

for path in $mounted_from_cloak; do
  mounts="$mounts -v $(pwd)/cloak/$path:/aircloak/cloak/$path"
done

for path in $mounted_from_cloak_cache; do
  mounts="$mounts -v $(pwd)/cloak/dev_container/cache/$path:/aircloak/cloak/$path"
done

CONTAINER_NAME="cloak_dev"
DOCKER_IMAGE="aircloak/cloak_dev"
DOCKER_START_ARGS="$mounts"

container_ctl $@
