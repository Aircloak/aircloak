#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh

init_env
. etcd_values_dev
