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

# needed to ensure that references from odbc.ini work properly
ln -nfs /aircloak/cloak/lib/cloak-$VERSION/priv /aircloak/cloak/priv

mkdir -p /aircloak/cloak/priv/odbc/drivers
if [ ! -z "$(ls /odbc_drivers)" ]; then cp -rp /odbc_drivers/* /aircloak/cloak/priv/odbc/drivers; fi

# fix the ownership to avoid some runtime errors
chown -R deployer:deployer /aircloak/cloak/

# symlinking runtime config after we've fixed the ownership, to prevent messing up mounted files
ln -sFf /runtime_config /aircloak/cloak/lib/cloak-$VERSION/priv/config

exec gosu deployer /aircloak/cloak/bin/cloak foreground
