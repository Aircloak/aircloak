#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh

build_aircloak_image cloak_dev cloak/dev_container/Dockerfile cloak/dev_container/.dockerignore
