#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

# This will build a dockerized version of the air-sandbox app.
# It has one major nasty caveat. Since we don't want to inlcude
# the SSH keys required to get at the private github repositories
# inside the docker containers (or any of the file system layers),
# we need to build two docker containers:
#
# 1. Build container with the correct erlang environment to build a release
# 2. Production image containing nothing but the release

function log {
  statement=$1
  echo "[aircloak] $statement"
}

function remove_dockerfile {
  log "Cleaning up Dockerfile"
  (rm .dockerignore) &> /dev/null
}
trap remove_dockerfile EXIT

function activate_stage {
  stage=$1
  if [ -L .dockerignore ]; then
    rm .dockerignore
  fi
  ln -s ".dockerignore-$stage" .dockerignore
  if [ -L Dockerfile ]; then
    rm Dockerfile
  fi
}


CWD=$(pwd)
./copy_configs.sh
mkdir -p artifacts/cache
cp -rp Makefile artifacts/cache
cp -rp rebar artifacts/cache
cp -rp rebar.config artifacts/cache
cp -rp rebar.config.lock artifacts/cache
cd artifacts/cache
make deps
cd $CWD/artifacts/cache/deps/cloak && git submodule update --init --recursive
cd $CWD

setup_env_init

log "Building the release"
activate_stage "builder"
docker build -t aircloak/air_backend_build:latest -f builder.dockerfile .

# Start the instance of the builder image and copy generated release back to the disk
builder_container_id=$(docker create aircloak/air_backend_build:latest)
mkdir -p artifacts/rel
rm -rf artifacts/rel/*
docker cp $builder_container_id:/tmp/build/rel/air - > ./artifacts/rel/air.tar.gz
docker stop $builder_container_id > /dev/null
docker rm -v $builder_container_id > /dev/null
cd artifacts/rel && tar -xzf air.tar.gz && cd ../..

activate_stage "release"
log "Creating release docker container"
docker build -t aircloak/air_backend:latest -f release.dockerfile .
push_to_registry air_backend
