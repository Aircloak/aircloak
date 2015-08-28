#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

. etcd_values_docker
if [ -f local_settings/docker ]; then
  . ./local_settings/docker
fi