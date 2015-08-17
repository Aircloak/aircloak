#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

init_env
. etcd_values_dev
if [ -f local_settings/dev ]; then
  . ./local_settings/dev
fi

# The test environment values might also have changed
./run-test-container.sh
