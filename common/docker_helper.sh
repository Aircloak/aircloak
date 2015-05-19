#!/usr/bin/env bash


function stop_named_container {
  if [ ! -z "$(docker ps --filter=name=$1 | grep -v CONTAINER)" ]; then
    echo "Stopping container $1"
    docker stop $1 > /dev/null
  fi

  if [ ! -z "$(docker ps -a --filter=name=$1 | grep -v CONTAINER)" ]; then
    echo "Removing container $1"
    docker rm $1 > /dev/null
  fi
}
