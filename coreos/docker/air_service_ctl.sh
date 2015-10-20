#!/usr/bin/env bash

set -eo pipefail

# Everything is enclosed into a block, which ensures that bash will load and
# parse the whole code. This allows the script to remove itself while uninstalling
# the system, and bash can still execute the code.
{
  # export air environment vars
  export $(cat /aircloak/air/environment | xargs)

  function start_system {
    remove_inactive_units

    start_local_service air-router
    start_local_service air-backend
    start_local_service air-frontend
    start_local_service air-frontend-discovery
  }

  function stop_system {
    stop_local_service air-frontend-discovery
    stop_local_service air-frontend
    stop_local_service air-backend
    stop_local_service air-router
  }

  function check_system {
    active_services=$(
          systemctl list-units |
          egrep 'air\-.*@' |
          grep 'active' |
          awk '{print $1}' |
          sed "s/@[0-9]*.service//" |
          sort |
          tr '\n' ' ' |
          sed 's/\s*$//' || true
        )
    if [ "$active_services" == "air-backend air-frontend air-frontend-discovery air-router" ]; then
      echo "yes"
    else
      echo "no"
    fi
  }

  function start_local_service {
    # determine the next id
    max_id=$(
          fleetctl list-units |
            grep "$1@" |
            awk '{print $1}' |
            sed "s/$1@//; s/\.service//" |
            sort |
            tail -n 1 || true
        )
    max_id=${max_id:-0}
    next_id=$((max_id + 1))

    echo "Starting $1@$next_id..."
    fleetctl start $1@$next_id
  }

  function stop_local_service {
    service=$(systemctl list-units | egrep "$1@.*\.service" | awk '{print $1}' || true)

    if [ "$service" != "" ]; then
      echo "Stopping $service"
      fleetctl stop "$service" || true
      fleetctl destroy "$service" || true
    fi
  }

  function upgrade_system {
    # stop local services
    stop_system

    # remove local images
    docker rmi $REGISTRY_URL/aircloak/air-installer || true
    docker rmi $REGISTRY_URL/aircloak/air_backend || true
    docker rmi $REGISTRY_URL/aircloak/air_router || true
    docker rmi $REGISTRY_URL/aircloak/air_frontend || true

    # remove air files
    rm -rf /aircloak/air

    # tail installation log
    follow_installation &

    # force reinstallation
    echo "Reinstalling system. This may take a while ..."
    systemctl restart air-installer.service

    # Make sure not to process until the installation has finished. In principle,
    # this shouldn't happen since restart is synchronous, but we do it just to be on the
    # safe side.
    while [ ! -e /aircloak/air/install/.installed ]; do
      echo "Installation not yet finished"
      sleep 2
    done

    # start the newly installed system
    start_system
  }

  function follow_installation {
    # tail installation log
    journalctl -f -n 200 -u air-installer&
    pid=$!

    while [ ! -e /aircloak/air/install/.installed ]; do sleep 2; done

    # kill background log process
    {
      kill -9 $pid && wait $pid 2>/dev/null
    } || true
  }

  function remove_inactive_units {
    for unit in $(inactive_units); do
      fleetctl destroy $unit
    done
  }

  function inactive_units {
    fleetctl list-unit-files | \
    egrep 'air\-.*@[0-9]+' | \
    grep inactive | \
    awk '{print $1}' || true
  }


  case "$1" in
    start_service)
      shift
      AIR_ENV=prod /aircloak/air/$1/container.sh foreground
      ;;

    stop_service)
      shift
      AIR_ENV=prod /aircloak/air/$1/container.sh stop
      ;;

    start_system)
      start_system
      ;;

    stop_system)
      stop_system
      ;;

    remove_inactive_units)
      remove_inactive_units
      ;;

    upgrade_system)
      upgrade_system
      ;;

    follow_installation)
      follow_installation
      ;;

    check_system)
      check_system
      ;;

    *)
      exit 1
      ;;
  esac

  exit 0
}