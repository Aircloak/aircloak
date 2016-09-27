#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../../docker/production_helper.sh
run_production_command air_db air/db air_db $1 publish
