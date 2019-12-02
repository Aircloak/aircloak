#!/bin/bash

set -eo pipefail

cd $(dirname $0)

./ldap/build-image.sh

echo "Starting LDAP"
LDAP_ENV="dev" ./ldap/container.sh ensure_started
LDAP_ENV="test" ./ldap/container.sh ensure_started
