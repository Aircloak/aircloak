#!/bin/bash

set -e

if [ -n "$(env | grep boot2docker)" ]; then
  # boot2docker: Create persistent folder and symlink
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
