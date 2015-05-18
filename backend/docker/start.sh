#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

function air_is_up {
  /aircloak/app/bin/air ping > /dev/null
  if [ $? -ne 0 ]; then
    return 1
  else
    return 0
  fi
}

export ETCD_HOST=${ETCD_HOST:-172.17.42.1}
export ETCD_PORT=${ETCD_PORT:-4002}

log "Booting container. Expecting etcd at http://$ETCD_HOST:$ETCD_PORT."

/aircloak/app/bin/air start

log "Waiting for air service to start"
until air_is_up; do
  sleep 1
done

log "Air service is running"
touch log/test.log
tail -f log/*.log
