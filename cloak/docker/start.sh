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
exec gosu deployer /aircloak/cloak/bin/cloak foreground
