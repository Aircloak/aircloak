#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

log "Booting container."

su postgres -c "mkdir -p /var/run/postgresql/9.6-main.pg_stat_tmp"
su postgres -c \
  "/usr/lib/postgresql/9.6/bin/pg_ctl -D /etc/postgresql/9.6/main -l /var/log/postgresql/postgresql.log start"

PRIV_DIR=/aircloak/air/lib/air-$VERSION/priv

mkdir -p ${PRIV_DIR}
ln -sFf /runtime_config ${PRIV_DIR}/config

exec gosu deployer /aircloak/air/bin/air foreground
