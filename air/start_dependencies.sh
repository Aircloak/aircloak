#!/bin/bash

set -eo pipefail

cd $(dirname $0)

./db/build-image.sh

echo "Starting databases for air"
DB_ENV="dev" ./db/container.sh ensure_started
DB_ENV="test" ./db/container.sh ensure_started

./ldap/build-image.sh

echo "Starting LDAP"
./ldap/container.sh ensure_started
