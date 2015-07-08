#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

REGISTRY_URL=${REGISTRY_URL:-""}

if [ "$REGISTRY_URL" != "" ]; then
  REGISTRY_URL="$REGISTRY_URL""/"
fi

if [ "$ETCD_PORT" != "" ]; then
  docker_env="-e ETCD_PORT=$ETCD_PORT"
fi

DOCKER_START_ARGS="-p 8080:8080 \
  $docker_env \
  -v $PWD/var-log:/var/log \
  -v $PWD/log:/aircloak/website/log \
  -v /aircloak/ca:/aircloak/ca \
  "$REGISTRY_URL"aircloak/air_frontend:latest \
  /aircloak/website/docker/start.sh"

REMOTE_CONSOLE_COMMAND="bundle exec rails c"

container_ctl air_frontend $@