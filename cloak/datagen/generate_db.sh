#!/usr/bin/env bash
set -e

cd $(dirname $0)

. ./prepare_db.funcs.sh

function db_command() {
  local db_name=$1
  local user_name=$2
  local cmd=$3

  psql -h $DB_HOST -p $DB_PORT -U $user_name $db_name -c "$cmd"
}

function generate_db() {
  local user_count=$1
  local db_name=$2
  local db_user_name=$3

  conditionally_create_user $db_name
  regenerate_database $db_name $db_user_name

  # Create the required tables
  db_command $db_name $db_user_name "CREATE TABLE users (id integer, name text, age integer, active boolean, height real);"
  db_command $db_name $db_user_name "CREATE TABLE users_encoded (id integer, name text, age text, active text, height text);"
  db_command $db_name $db_user_name "CREATE TABLE notes (id integer, user_id integer, title text, content text);"
  db_command $db_name $db_user_name "CREATE TABLE notes_encoded (id integer, user_id integer, title text, content text);"
  db_command $db_name $db_user_name "CREATE TABLE drafts_changes (id integer, note_id integer, \"changes.date\" timestamp, \"changes.change\" text);"
  db_command $db_name $db_user_name "CREATE TABLE drafts_changes_encoded (id integer, note_id integer, \"changes.date\" text, \"changes.change\" text);"
  db_command $db_name $db_user_name "CREATE TABLE addresses (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" integer, \"work.city\" text, \"work.postal_code\" integer);"
  db_command $db_name $db_user_name "CREATE TABLE addresses_encoded (id integer, user_id integer, \"home.city\" text, \"home.postal_code\" text, \"work.city\" text, \"work.postal_code\" text);"

  # Generate the dataset
  mkdir -p output
  ruby compliance_data_generator.rb --users $user_count

  # Copy the data into the database
  db_command $db_name $db_user_name "\copy users FROM '$PWD/output/users.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy users_encoded FROM '$PWD/output/users_encoded.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy notes FROM '$PWD/output/notes.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy notes_encoded FROM '$PWD/output/notes_encoded.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy drafts_changes FROM '$PWD/output/drafts_changes.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy drafts_changes_encoded FROM '$PWD/output/drafts_changes_encoded.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy addresses FROM '$PWD/output/addresses.csv' delimiter ',' csv header;"
  db_command $db_name $db_user_name "\copy addresses_encoded FROM '$PWD/output/addresses_encoded.csv' delimiter ',' csv header;"
}

export USER_COUNT=${USER_COUNT:-100}
export DB_HOST=${DB_HOST:-127.0.0.1}
export DB_PORT=${DB_PORT:-5432}
export DB_NAME=${DB_NAME:-datasource_compliance}
generate_db $USER_COUNT $DB_NAME $DB_NAME
