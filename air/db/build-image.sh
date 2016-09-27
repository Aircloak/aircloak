#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh

build_aircloak_image air_db air/db/Dockerfile air/db/.dockerignore
