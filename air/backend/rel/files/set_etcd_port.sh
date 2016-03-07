#!/bin/bash

set -eo pipefail

. $(dirname ${BASH_SOURCE[0]})/config.sh
export ETCD_CLIENT_PORT=$(get_tcp_port $1 etcd/client)
