#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh
. ../config/config.sh

init_env prod

. etcd_values_coreos
if [ -f local_settings/coreos ]; then
  . ./local_settings/coreos
fi