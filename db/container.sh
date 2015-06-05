#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

container_ctl air_db "$@" \
  -v /docker_volumes/air_db:/var/lib/postgresql/data \
  -p 5433:5432 \
  aircloak/air_db:latest
