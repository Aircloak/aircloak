#!/usr/bin/env bash

set -eo pipefail

# Compute machines suffix in form of {1,2,...} or just 1 (for a single machine cluster)
machines_num=$(($(fleetctl list-machines | wc -l) - 1))
service_indices="$(seq 1 $machines_num | paste -sd "," -)"
if [ $machines_num -gt 1 ]; then service_indices="{$service_indices}"; fi

fleetctl submit /aircloak/air/router@.service
fleetctl start "router@$service_indices"
