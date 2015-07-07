#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

# We don't want all our production
# secrets leaking out in the console
# during deployment, where any bystander
# can see them.
silence_etcd_set

. etcd_values_prod
if [ -f local_settings/prod ]; then
  . ./local_settings/prod
fi
