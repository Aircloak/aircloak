#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

log "Booting container."

su postgres -c "mkdir -p /var/run/postgresql/11-main.pg_stat_tmp"
su postgres -c \
  "/usr/lib/postgresql/11/bin/pg_ctl -D /etc/postgresql/11/main -l /var/log/postgresql/postgresql.log start"

PRIV_DIR=/aircloak/air/lib/air-$VERSION/priv

mkdir -p ${PRIV_DIR}
ln -sFf /runtime_config ${PRIV_DIR}/config

if [ "$CRASH_DUMP" == "true" ]; then
  mkdir -p /crash_dump
  chmod -R o+w /crash_dump
  export ERL_CRASH_DUMP="/crash_dump/erl_crash.dump"
else
  export ERL_CRASH_DUMP="/dev/null"
fi

exec gosu deployer /aircloak/air/bin/air foreground
