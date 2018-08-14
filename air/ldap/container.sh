#!/bin/bash

set -eo pipefail

cd $(dirname $0)
. ../../docker/docker_helper.sh

if [ "$LDAP_ENV" == "test" ]; then
  DOCKER_START_ARGS="
    -e LDAP_TLS_VERIFY_CLIENT=allow
    -e LDAP_TLS_CRT_FILENAME=localhost.crt
    -e LDAP_TLS_KEY_FILENAME=localhost.key
    -e LDAP_TLS_CA_CRT_FILENAME=ca.crt
    -p 389:389
    -p 636:636
  "
elif [ "$LDAP_ENV" == "dev" ]; then
  DOCKER_START_ARGS="
    -e LDAP_TLS_VERIFY_CLIENT=allow
    -e LDAP_TLS_CRT_FILENAME=localhost.crt
    -e LDAP_TLS_KEY_FILENAME=localhost.key
    -e LDAP_TLS_CA_CRT_FILENAME=ca.crt
    -p 1389:389
    -p 1636:636
  "
fi

DOCKER_IMAGE="aircloak/ldap"
DOCKER_IMAGE_VERSION="latest"
CONTAINER_NAME="aircloak_ldap_${LDAP_ENV}"

container_ctl $@
