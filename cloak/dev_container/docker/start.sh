#!/bin/bash

set -eo pipefail

# We're redirecting localhost to point to the macOS docker host. This allows us to use the same
# dev.json and config.json and still be able to connect to the data sources.
host_ip=$(getent hosts docker.for.mac.localhost | awk '{ print $1 }')
etc_hosts_contents=$(cat /etc/hosts | sed "s/127\.0\.0\.1\\s*localhost/$host_ip localhost/")
echo "$etc_hosts_contents" > /etc/hosts

cp /aircloak/cloak/priv/odbc/docker/odbc.ini /etc/

cd /aircloak/cloak
PROMPT_COMMAND='history -a' bash || true
