#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

log "Booting container."
mkdir -p /aircloak/central/lib/central-0.0.1/priv/config/
cp -rp /runtime_config/* /aircloak/central/lib/central-0.0.1/priv/config/
exec gosu deployer /aircloak/central/bin/central foreground
