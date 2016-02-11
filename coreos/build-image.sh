#!/usr/bin/env bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/../common/docker_helper.sh

build_aircloak_image air_installer coreos
