#!/bin/bash

set -eo pipefail

cd $(dirname $0)

./db/build-image.sh
DB_ENV="dev" ./db/container.sh ensure_started
DB_ENV="test" ./db/container.sh ensure_started
