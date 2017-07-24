#!/usr/bin/env bash
set -e

cd $(dirname $0)

function main() {
  echo "-------- Creating data --------"
  ruby ./gen_bank_data.rb > bank_data.sql
  echo "-------- Importing data --------"
  psql -h $DB_HOST -p $DB_PORT -U postgres cloak < bank_data.sql
  echo "-------- Cleaning up --------"
  rm bank_data.sql
}

export DB_HOST=${DB_HOST:-127.0.0.1}
export DB_PORT=${DB_PORT:-5432}
(main)
