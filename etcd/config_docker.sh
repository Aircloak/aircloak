#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

init_env
cat etcd_values_dev \
  | sed "s/\.air\-local:8203/.air-local:8201/; s#../router/dev_cert#/aircloak/ca#" \
  > ./etcd_values_docker
. etcd_values_docker
if [ -f local_settings/docker ]; then
  . ./local_settings/docker
fi