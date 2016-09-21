#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export HTTP_HOST_IP=${AIR_HOST_NAME:-"127.0.0.1"}

log "Booting container."
mkdir -p /aircloak/air/lib/air-0.0.1/priv/config/
cp -rp /runtime_config/* /aircloak/air/lib/air-0.0.1/priv/config/
exec gosu deployer /aircloak/air/bin/air foreground
