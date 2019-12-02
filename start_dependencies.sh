#!/bin/bash

set -eo pipefail

cd $(dirname $0)

./db/build-image.sh

echo "Starting database"
DB_ENV="dev" ./db/container.sh ensure_started

air/start_dependencies.sh
central/start_dependencies.sh
cloak/start_dependencies.sh
