#!/bin/sh
set -e

database=$(cat /runtime_config/config.json | jq --raw-output ".database.name")
user=$(cat /runtime_config/config.json | jq --raw-output ".database.user")
password=$(cat /runtime_config/config.json | jq --raw-output ".database.password")

psql -U "$POSTGRES_USER" -c "CREATE USER $user WITH PASSWORD '${password}' CREATEDB;"
psql -U "$POSTGRES_USER" -c "CREATE DATABASE $database ENCODING 'UTF8';"
psql -U "$POSTGRES_USER" -c "GRANT ALL PRIVILEGES ON DATABASE $database TO $user;"
psql -U "$POSTGRES_USER" -c "ALTER DATABASE $database OWNER TO $user;"
