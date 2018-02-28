#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../../docker/production_helper.sh

run_production_command performance_db cloak/performance_db performance_db $1 publish
