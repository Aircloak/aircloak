#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh
stop_named_container air_backend

docker run --rm --name air_backend -it -p 11000:11000 -p 9000:9000 "$@" aircloak/air:latest
