#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

init_env
cat etcd_values_dev | sed s/127.0.0.1/172.17.42.1/ > ./etcd_values_docker
echo 'etcd_set /service/frontend/endpoint "http://${GATEWAY}:8080"' >> ./etcd_values_docker
. etcd_values_docker
if [ -f local_settings/docker ]; then
  . ./local_settings/docker
fi