#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ./etcd/etcd_lib.sh

function stop_docker_services {
  frontend/container.sh stop&
  backend/container.sh stop&
  router/container.sh stop&
  wait
}

function start_docker_service {
  $1/build-image.sh
  $1/container.sh start
}

case "$1" in
  stop)
    stop_docker_services
    ;;

  start)
    stop_docker_services
    ./start_dependencies.sh
    start_docker_service frontend &
    start_docker_service backend &
    start_docker_service router &
    wait
    export ETCD="127.0.0.1:4002"
    printf "\nYou can access the site at https://frontend.air-local:$(etcd_get /tcp_ports/router/https)\n\n"
    ;;

  *)
    echo "$(basename $0) start|stop"
    exit 1
    ;;

esac