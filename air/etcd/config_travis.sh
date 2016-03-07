#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh
. ../config/config.sh

init_env test

. ./etcd_values_test

# overrides
etcd_set /settings/air/db/port 5432
