#!/bin/bash

set -eo pipefail

cd $(dirname $0)

# We always restart etcd (since settings may have changed). In contrast, db and
./db/build-image.sh && ./db/container.sh ensure_started
