#!/usr/bin/env bash

set -eo pipefail

function start_system {
  remove_inactive_units

  start_service_instance air-router
  start_service_instance air-backend
  start_service_instance air-frontend
  start_service_instance air-frontend-discovery
}

function start_service_instance {
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


# export air environment vars
export $(cat /aircloak/air/environment | xargs)

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

  remove_inactive_units)
    remove_inactive_units
    ;;

  *)
    printf "\nUsage:\n  $0 start_service | stop_service | start_system | remove_inactive_units\n\n"
    exit 1
    ;;
esac