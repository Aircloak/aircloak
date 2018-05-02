#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
. ../docker/docker_helper.sh

./db/build-image.sh

echo "Starting databases for central"
DB_ENV="dev" ./db/container.sh ensure_started
DB_ENV="test" ./db/container.sh ensure_started
