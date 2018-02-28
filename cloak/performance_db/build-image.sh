#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh

build_aircloak_image performance_db cloak/performance_db/Dockerfile cloak/performance_db/.dockerignore
