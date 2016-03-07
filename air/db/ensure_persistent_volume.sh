#!/bin/bash

set -e

if [ -n "$DOCKER_MACHINE_NAME" ]; then
  docker-machine ssh $DOCKER_MACHINE_NAME << EOF
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
