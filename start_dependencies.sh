#!/bin/bash

set -eo pipefail

cd $(dirname $0)

echo "Building containers"
./air/db/build-image.sh
./central/db/build-image.sh

echo "Starting databases for air"
DB_ENV="dev" ./air/db/container.sh ensure_started
DB_ENV="test" ./air/db/container.sh ensure_started

echo "Starting databases for central"
DB_ENV="dev" ./central/db/container.sh ensure_started
DB_ENV="test" ./central/db/container.sh ensure_started
