#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

init_env
cat etcd_values_dev \
  | sed "s/127.0.0.1/172.17.42.1/g; s/\.local:8203/.local:8201/" \
  > ./etcd_values_docker
. etcd_values_docker
if [ -f local_settings/docker ]; then
  . ./local_settings/docker
fi