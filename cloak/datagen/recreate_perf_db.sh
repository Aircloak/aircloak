#!/usr/bin/env bash
set -e

cd $(dirname $0)

. ./prepare_db.funcs.sh

DB_HOST=${DB_HOST:-127.0.0.1}
DB_PORT=${DB_PORT:-20002}

conditionally_create_user "performance"

regenerate_database "performance" "performance"

cd ../
mix gen.test_data perf 10000
mix gen.data_source_config perf priv/config/perf
