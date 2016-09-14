#!/bin/bash

set -eo pipefail

function log {
  msg=$1
  echo "[aircloak] $msg"
}

export HTTP_HOST_IP=${AIR_HOST_NAME:-"127.0.0.1"}
export AIR_INSIGHTS_ENV="prod"

log "Booting container."
mkdir -p /aircloak/insights/lib/air-0.0.1/priv/config/
cp -rp /runtime_config/* /aircloak/insights/lib/air-0.0.1/priv/config/
exec gosu deployer /aircloak/insights/bin/air foreground
