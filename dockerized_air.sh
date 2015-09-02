#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ./config/config.sh

function stop_docker_services {
  frontend/container.sh stop&
  backend/container.sh stop&
  router/container.sh stop&
  balancer/container.sh stop&
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
    start_docker_service balancer &
    wait
    printf "\nYou can access the site at:\n"
    printf "  https://frontend.air-local:$(get_tcp_port prod router/https) (router endpoint)\n"
    printf "  https://frontend.air-local:$(get_tcp_port prod balancer/https) (balancer endpoint)\n\n"
    ;;

  *)
    echo "$(basename $0) start|stop"
    exit 1
    ;;

esac