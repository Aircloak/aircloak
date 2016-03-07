Air db
============

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Running](#running)
    - [Migrating data](#migrating-data)

----------------------

# What it does

This component is meant to run only on the development machine. It runs a Postgresql server with two databases: development and test.

# Getting started

## Running

Just run `db/container.sh start`. This will start the server, and if needed create the corresponding users and
databases. Database server listens on the port 20002.

Database migration is performed by the [frontend component](../frontend/README.md#running).

## Migrating data

In case you previously hosted local database and want to preserve that data, you can perform a one-off migration to the docker database:

1. Make sure etcd and database containers are running
2. OS X users: make sure port 20002 is forwarded from your localhost to docker-machine VM.
3. Make sure that both docker and your local databases are migrated. Migrations are done from the `frontend` folder by running `bundle exec rake db:migrate`. You can migrate your local database by temporary hardcoding database settings in `database.yml` and running the migration. Then revert back `database.yml` and run migration once more to migrate the docker database. Finally, make sure to run `RAILS_ENV=test bundle exec rake db:migrate` to migrate the test database.
4. On your localhost run
   `pg_dump --data-only --no-owner --no-acl -h localhost -U postgres localhost_db_name | psql -h 127.0.0.1 -p 20002 -U postgres aircloakdatabase`
   This assumes that your old database is called `localhost_db_name`. If that's not the case, change the name in the command. You'll see various errors, but data should be migrated. You can check that manually by inspecting the content of some table on `localhost:20002` database server.

__OS X developers__: You should be aware that your database data is stored inside docker-machine VM in
`/mnt/sda1/docker_volumes/air_db` folder. This data persists after restarts of docker-machine (or the host itself). However, __if you delete docker-machine VM, this data will be lost.__
