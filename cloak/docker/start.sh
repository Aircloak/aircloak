#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

# needed only for local testing
echo "127.0.0.1 insights.air-local" >> /etc/hosts

log "Booting container."
mkdir -p /aircloak/cloak/lib/cloak-$VERSION/priv/
ln -sFf /runtime_config /aircloak/cloak/lib/cloak-$VERSION/priv/config

# symlinking mounted persist folder to cloak, and giving write permissions, so the deployer user can write to it
ln -sFf /persist /aircloak/cloak/lib/cloak-$VERSION/priv/persist
chmod -R o+w /persist

# needed to ensure that references from odbc.ini work properly
ln -nfs /aircloak/cloak/lib/cloak-$VERSION/priv /aircloak/cloak/priv

# setup external ODBC drivers
drivers_path="/aircloak/cloak/priv/odbc/drivers"
mkdir -p "$drivers_path"
for driver in /odbc_drivers/*; do
  if [ -f "$driver/setup.sh" ]; then
    pushd "$driver"
    . "$driver/setup.sh"
    popd
  else
    name=`basename $driver`
    rm -rf "$drivers_path/$name"
    ln -s "$driver" "$drivers_path/$name"
  fi
done

exec gosu deployer /aircloak/cloak/bin/cloak foreground
