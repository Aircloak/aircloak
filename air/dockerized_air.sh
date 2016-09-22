#!/bin/bash

set -eo pipefail

cd $(dirname $0)

function stop_docker_services {
  site/container.sh stop&
  router/container.sh stop&
  balancer/container.sh stop&
  wait
}

function start_docker_service {
  $1/build-image.sh
  $1/container.sh ensure_started
}

case "$1" in
  stop)
    stop_docker_services
    ;;

  start)
    ./start_dependencies.sh --no-router
    start_docker_service site
    start_docker_service router
    start_docker_service balancer

    printf "\nYou can access the site at:\n"
    printf "  https://insights.air-local:$(get_tcp_port prod balancer/https)\n\n"
    ;;

  *)
    echo "$(basename $0) start|stop"
    exit 1
    ;;

esac
