#!/bin/bash

set -eo pipefail

host_ip=$(getent hosts docker.for.mac.localhost | awk '{ print $1 }')
echo "$host_ip insights.air-local" >> /etc/hosts
cp /aircloak/cloak/priv/odbc/docker/odbc.ini /etc/

cd /aircloak/cloak
PROMPT_COMMAND='history -a' bash || true
