#!/usr/bin/env bash
set -e

cd $(dirname $0)

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
  psql -h $DB_HOST -p $DB_PORT -U postgres cloak < dev_data.sql

  cd ../
  mix gen.dev_data
  mix gen.test_data "compliance" 200
}

export DB_HOST=${DB_HOST:-127.0.0.1}
export DB_PORT=${DB_PORT:-5432}
(main)
