#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

log "Booting container."
mkdir -p /aircloak/air/lib/air-$VERSION/priv/config/
cp -rp /runtime_config/* /aircloak/air/lib/air-$VERSION/priv/config/
exec gosu deployer /aircloak/air/bin/air foreground
