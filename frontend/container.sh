#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../common/docker_helper.sh

container_ctl air_frontend \
  "$@" -p 8080:8080 -v $PWD/var-log:/var/log -v $PWD/log:/aircloak/website/log \
  aircloak/air_frontend:latest
