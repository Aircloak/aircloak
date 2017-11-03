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

  echo "$user_command" | psql -h $DB_HOST -p $DB_PORT -U postgres
}

regenerate_database()
{
  database=$1
  user=$2
  psql -h $DB_HOST -p $DB_PORT -c "DROP DATABASE IF EXISTS $database;" -U postgres
  psql -h $DB_HOST -p $DB_PORT -c "CREATE DATABASE $database ENCODING 'UTF8';" -U postgres
  psql -h $DB_HOST -p $DB_PORT -c "GRANT ALL PRIVILEGES ON DATABASE $database TO $user;" -U postgres
}

initialize_database()
{
  database=$1
  user=$2

  conditionally_create_user $database
  regenerate_database $database $user
}
