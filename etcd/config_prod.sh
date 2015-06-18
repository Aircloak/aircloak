#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

. etcd_values_prod
if [ -f local_settings/prod ]; then
  . ./local_settings/prod
fi