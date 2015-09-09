#!/usr/bin/env bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/../common/docker_helper.sh

build_aircloak_image air_balancer balancer
push_to_registry air_balancer
