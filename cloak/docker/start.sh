#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# needed only for local testing
echo "127.0.0.1 insights.air-local" >> /etc/hosts

log "Booting container."

PRIV_DIR=/aircloak/cloak/lib/cloak-$VERSION/priv

mkdir -p ${PRIV_DIR}
ln -sFf /runtime_config ${PRIV_DIR}/config

# symlinking mounted persist folder to cloak, and giving write permissions, so the deployer user can write to it
ln -sFf /persist ${PRIV_DIR}/persist
chmod -R o+w /persist

# needed to ensure that references from odbc.ini work properly
ln -nfs ${PRIV_DIR} /aircloak/cloak/priv

# setup external ODBC drivers
drivers_path="/aircloak/cloak/priv/odbc/drivers"
mkdir -p "$drivers_path"
for driver in /odbc_drivers/*; do
  if [ -f "$driver/setup.sh" ]; then
    pushd "$driver"
    . setup.sh
    popd
  else
    name=`basename $driver`
    rm -rf "$drivers_path/$name"
    ln -s "$driver" "$drivers_path/$name"
  fi
done

exec gosu deployer /aircloak/cloak/bin/cloak foreground
