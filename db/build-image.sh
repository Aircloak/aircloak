#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/..

. docker/docker_helper.sh

build_aircloak_image aircloak_db db/Dockerfile db/.dockerignore
