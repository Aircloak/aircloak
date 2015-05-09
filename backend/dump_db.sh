#!/usr/bin/env bash

cat <<EOF > test/db_structure.sql
-- Auto generated file, please don't modify.
-- To regenerate this file, make sure your aircloakdatabase is up to date, and
-- then run ./dump_db.sh

EOF

pg_dump aircloakdatabase -S air -s -x -O >> test/db_structure.sql
