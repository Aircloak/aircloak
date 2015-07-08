#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

AIR_ETCD_PORT=4001
ETCD_HOST_IP=127.0.0.1

init_env

. etcd_values_coreos
if [ -f local_settings/coreos ]; then
  . ./local_settings/coreos
fi