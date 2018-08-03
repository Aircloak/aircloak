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

mkdir -p /aircloak/cloak/priv/odbc/drivers
ln -s /opt/mapr/drill /aircloak/cloak/priv/odbc/drivers/drill
if [ ! -z "$(ls /odbc_drivers)" ]; then cp -rp /odbc_drivers/* /aircloak/cloak/priv/odbc/drivers; fi

exec gosu deployer /aircloak/cloak/bin/cloak foreground
