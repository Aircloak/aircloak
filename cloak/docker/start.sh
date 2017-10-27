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

# By replacing the config folder in cloak app with a symlink to the mounted folder,
# we can support live reloading of data source definition files.
rm -rf /aircloak/cloak/lib/cloak-$VERSION/priv/config
ln -s /runtime_config /aircloak/cloak/lib/cloak-$VERSION/priv/config
exec gosu deployer /aircloak/cloak/bin/cloak foreground
