#!/usr/bin/env bash
set -e

cd $(dirname $0)

USER_COUNT=$1

. ./prepare_db.funcs.sh

function compliance_db_command() {
  cmd=$1
  psql -h $DB_HOST -p $DB_PORT -U datasource_compliance datasource_compliance -c "$cmd"
}

function generate_compliance_db() {
  conditionally_create_user "datasource_compliance"
  regenerate_database "datasource_compliance" "datasource_compliance"

  # Create the required tables
  compliance_db_command "CREATE TABLE users (id integer, name text, age integer, active boolean, height real);"
  compliance_db_command "CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);"
  compliance_db_command "CREATE TABLE notes (id integer, user_id integer, title text, content text);"
  compliance_db_command "CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);"
  compliance_db_command "CREATE TABLE drafts_changes (id integer, note_id integer, \"changes.date\" timestamp, \"changes.change\" text);"
  compliance_db_command "CREATE TABLE drafts_changes_encoded (id integer, note_id integer, \"changes.date\" text, \"changes.change\" text);"
  compliance_db_command "CREATE TABLE addresses (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" integer, \"work.city\" text, \"work.postal_code\" integer);"
  compliance_db_command "CREATE TABLE addresses_encoded (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" text, \"work.city\" text, \"work.postal_code\" text);"

  # Generate the dataset
  mkdir -p output
  ruby compliance_data_generator.rb --users $USER_COUNT

  # Copy the data into the database
  compliance_db_command "\copy users FROM '$PWD/output/users.csv' delimiter ',' csv header;"
  compliance_db_command "\copy users_encoded FROM '$PWD/output/users_encoded.csv' delimiter ',' csv header;"
  compliance_db_command "\copy notes FROM '$PWD/output/notes.csv' delimiter ',' csv header;"
  compliance_db_command "\copy notes_encoded FROM '$PWD/output/notes_encoded.csv' delimiter ',' csv header;"
  compliance_db_command "\copy drafts_changes FROM '$PWD/output/drafts_changes.csv' delimiter ',' csv header;"
  compliance_db_command "\copy drafts_changes_encoded FROM '$PWD/output/drafts_changes_encoded.csv' delimiter ',' csv header;"
  compliance_db_command "\copy addresses FROM '$PWD/output/addresses.csv' delimiter ',' csv header;"
  compliance_db_command "\copy addresses_encoded FROM '$PWD/output/addresses_encoded.csv' delimiter ',' csv header;"
}

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

  generate_compliance_db

  cd ../
  mix gen_dev_data
}

export DB_HOST=${DB_HOST:-127.0.0.1}
export DB_PORT=${DB_PORT:-5432}
(main)
