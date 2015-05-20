#!/bin/sh
set -e

conditionally_create_user()
{
  user=$1
  user_command='
  DO LANGUAGE plpgsql
  $body$
  BEGIN
    IF NOT EXISTS (
        SELECT *
        FROM pg_catalog.pg_user
        WHERE usename = '\'"${user}"\'') THEN

      CREATE USER '"${user}"';
    END IF;
  END
  $body$'

  echo "$user_command" | psql -U postgres
}

conditionally_create_database()
{
  database=$1
  user=$2

  count=$(psql -lqt -U postgres | cut -d \| -f 1 | grep -w $database | wc -l)
  if [ $count -eq 0 ]; then
    psql -c "CREATE DATABASE $database ENCODING 'UTF8';" -U postgres
    psql -c "GRANT ALL PRIVILEGES ON DATABASE $database TO $user;" -U postgres
  fi
}

server_is_up() {
  if eval "echo 'select 1' | psql -U postgres > /dev/null 2>&1"; then
    return 0
  else
    return 1
  fi
}

until server_is_up; do
  echo "Postgresql server not yet running..."
  sleep 1
done

conditionally_create_user "air"
conditionally_create_database "aircloakdatabase" "air"

conditionally_create_user "airtest"
psql -U postgres -c "ALTER USER airtest CREATEDB"
conditionally_create_database "air_test_database" "airtest"
psql -U postgres -c "ALTER DATABASE air_test_database OWNER TO airtest"
