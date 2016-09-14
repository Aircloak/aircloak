#!/bin/bash

set -eo pipefail

cd $(dirname $0)

# We always restart etcd (since settings may have changed). In contrast, db and
# docker_registry are not restarted since these change practically never.
./db/build-image.sh && ./db/container.sh ensure_started
./docker_registry/container.sh ensure_started
