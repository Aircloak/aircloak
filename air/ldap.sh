#!/bin/bash

set -e

cd $(dirname $0)
. ../docker/docker_helper.sh

DOCKER_IMAGE="osixia/openldap"
DOCKER_IMAGE_VERSION="1.2.1"
DOCKER_START_ARGS="
  -e LDAP_TLS_VERIFY_CLIENT=allow
  -p 389:389
  -p 636:636
"
CONTAINER_NAME="aircloak_ldap"

container_ctl $@
