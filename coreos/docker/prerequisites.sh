#!/usr/bin/env bash

set -eo pipefail

echo "waiting for air keys"

while
  [ ! -e /aircloak/ca/acinfra.aircloak.com.pem ] ||
  [ ! -e /aircloak/ca/aircloak.com.chain.pem ] ||
  [ ! -e /aircloak/ca/api.cert ] ||
  [ ! -e /aircloak/ca/api.key ]; do
    sleep 5;
done && echo "got air keys"