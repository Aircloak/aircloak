#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh

build_aircloak_image central_db central/db/Dockerfile central/db/.dockerignore
