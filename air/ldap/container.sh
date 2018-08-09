#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ../../docker/docker_helper.sh

DOCKER_IMAGE="aircloak/ldap"
DOCKER_IMAGE_VERSION="latest"
DOCKER_START_ARGS="
  -e LDAP_TLS_VERIFY_CLIENT=allow
  -p 389:389
  -p 636:636
"
CONTAINER_NAME="aircloak_ldap"

container_ctl $@
