#!/bin/bash

# Migrating the localhost database contents to the docker:
#
# 1. Start this container
# 2. OS X users: make sure port 5433 is mapped from boot2docker to your Mac
# 3. Make sure that both docker and your local databases are migrated
# 4. On your machine (outside of any VMs) execute
#      pg_dump -C -h localhost -U postgres aircloakdatabase | psql -h 127.0.0.1 -p 5433 -U postgres aircloakdatabase
#    (there will be various errors, but you can ignore them)

set -e

cd $(dirname $0)

docker stop air_db || true
docker rm air_db || true

docker build -t aircloak/air_db:latest .

if [ -n "$(env | grep boot2docker)" ]; then
  # boot2docker: Create persistend folder and symlink
  B2D_IP=$(boot2docker ip &> /dev/null)
  if [ "$?" != "0" ]
  then
    boot2docker up
    $(boot2docker shellinit)
    B2D_IP=$(boot2docker ip &> /dev/null)
  fi

  boot2docker ssh << EOF
    sudo mkdir -p /mnt/sda1/docker_volumes/air_db
    if [ ! -e /docker_volumes ]; then
      sudo ln -ns /mnt/sda1/docker_volumes /docker_volumes
    fi
    exit
EOF
else
  if [ ! -e /docker_volumes ]; then
    echo "Need to create local folder /docker_volumes/air_db. Sudo password needed."
    sudo mkdir -p /docker_volumes/air_db
  fi
fi

echo "Starting container..."
docker run \
  --name air_db -d -i \
  -v /docker_volumes/air_db:/var/lib/postgresql/data \
  -p 5433:5432 \
  aircloak/air_db:latest

pg_status=$(docker exec air_db gosu postgres pg_ctl status | grep 'is running')
if [ -n pg_status ]; then
  docker exec air_db /init_db.sh

  echo
  echo "Database server is running and listening on port 5433."
  echo "If you're running on OS X, make sure map the boot2docker port 5433 to your localhost."
  echo
else
  echo "Container has not been started successfully."
  exit 1
fi
