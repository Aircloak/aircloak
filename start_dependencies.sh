#!/bin/bash

set -eo pipefail

cd $(dirname $0)

./etcd/container.sh start
./db/container.sh start
./docker_registry/container.sh start
./balancer/run_local.sh
