#!/usr/bin/env bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/../common/docker_helper.sh

build_production_image air-installer coreos
