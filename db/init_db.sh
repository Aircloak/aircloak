#!/bin/sh
set -e

for config in /runtime_config/$DB_ENV/*.json; do
  database=$(cat $config | jq --raw-output ".database.name")
  user=$(cat $config | jq --raw-output ".database.user")
  password=$(cat $config | jq --raw-output ".database.password")

  psql -U "$POSTGRES_USER" -c "CREATE USER $user WITH PASSWORD '${password}' CREATEDB;"
  psql -U "$POSTGRES_USER" -c "CREATE DATABASE $database ENCODING 'UTF8' LC_CTYPE = 'en_US.UTF-8' TEMPLATE template0;"
  psql -U "$POSTGRES_USER" -c "GRANT ALL PRIVILEGES ON DATABASE $database TO $user;"
  psql -U "$POSTGRES_USER" -c "ALTER DATABASE $database OWNER TO $user;"
done
