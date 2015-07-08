#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

STOP_SIGNAL=SIGQUIT
STOP_TIMEOUT=30

DOCKER_START_ARGS="-p 8080:8080 \
  -v $PWD/var-log:/var/log \
  -v $PWD/log:/aircloak/website/log \
  -v /aircloak/ca:/aircloak/ca \
  aircloak/air_frontend:latest \
  /aircloak/website/docker/start.sh"

container_ctl air_frontend $@