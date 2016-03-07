#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)
. ../config/config.sh

printf $(get_tcp_port $1 etcd/client)
