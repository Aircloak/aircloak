#!/bin/bash

set -e

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
  (rm Dockerfile .dockerignore) &> /dev/null
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
  ln -s "Dockerfile-$stage" Dockerfile
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

log "Creating build image"
activate_stage "builder"
docker build -t aircloak/air_backend_build:latest .
log "Performing build inside build container"
docker run -v $PWD:/aircloak/source --rm aircloak/air_backend_build:latest /aircloak/utils/build.sh

# Now we should have a build in the artifacts/cache folder
activate_stage "release"
log "Creating release docker container"
docker build -t aircloak/air_backend:latest .

if named_container_running air_docker_registry; then
  log "Pushing to local registry"
  docker tag -f aircloak/air_backend:latest localhost:5000/aircloak/air_backend:latest
  docker push localhost:5000/aircloak/air_backend:latest
else
  echo "Warning: local registry is not running, image not pushed."
fi
