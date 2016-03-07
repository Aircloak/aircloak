#!/bin/bash

set -eo pipefail

cd $(dirname $0)

cp -rp ../config/config.sh rel/files
cp -rp ../config/tcp_ports.json rel/files
