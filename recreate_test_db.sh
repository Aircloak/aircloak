#!/usr/bin/env bash

set -e

. ./deps/cloak/prepare_db.funcs.sh

conditionally_create_user "airtest"
regenerate_database "air_test_database" "airtest"

cat test/db_structure.sql | psql -U airtest air_test_database
