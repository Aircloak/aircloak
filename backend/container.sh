#!/bin/bash

set -e

cd $(dirname $0)
. ../common/docker_helper.sh

container_ctl air_backend "$@" -p 11000:11000 -p 9000:9000 aircloak/air_backend:latest
