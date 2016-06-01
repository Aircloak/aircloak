#!/usr/bin/env bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/../common/docker_helper.sh
cd $(dirname ${BASH_SOURCE[0]})/..

build_aircloak_image air_balancer balancer
