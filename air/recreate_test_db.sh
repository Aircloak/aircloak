#!/usr/bin/env bash

set -e

. backend/deps/cloak/prepare_db.funcs.sh

conditionally_create_user "airtest"
psql -U postgres -c "ALTER USER airtest CREATEDB"

regenerate_database "air_test_database" "airtest"
psql -U postgres -c "ALTER DATABASE air_test_database OWNER TO airtest"
