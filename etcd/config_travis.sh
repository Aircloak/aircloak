#!/usr/bin/env bash

set -e

cd $(dirname $0)
. ./etcd_lib.sh
. ../config/config.sh

export ETCD_PORT=4004
export HOST_IP="127.0.0.1"
export ETCD=$HOST_IP:$ETCD_PORT

cat etcd_values_test | sed s/$(get_tcp_port prod database/tcp)/5432/ > ./etcd_values_travis
. etcd_values_travis
