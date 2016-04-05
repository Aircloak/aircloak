#!/usr/bin/env bash
set -e

. ./prepare_db.funcs.sh

function main() {
  conditionally_create_user "cloak"
  conditionally_create_user "cloaktest1"
  conditionally_create_user "cloaktest2"
  conditionally_create_user "cloaktest3"

  regenerate_database "cloak" "cloak"
  regenerate_database "cloaktest1" "cloaktest1"
  regenerate_database "cloaktest2" "cloaktest2"
  regenerate_database "cloaktest3" "cloaktest3"

  # import dev schema and data
  psql -U postgres cloak < dev_data.sql
}

(main)
