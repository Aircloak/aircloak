#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../../docker/production_helper.sh
run_production_command central/db central_db $1 publish
