#!/bin/bash

set -eo pipefail

# This script runs periodically as root to fix permissions. The problem is that docker containers generates files
# in the mounted volumes, and these files are owned by the root. Therefore, the cleanup logic can't remove the stale
# folders. This script will be executed from the periodic systemd service to assign the CI user as the owner.

{
  find /home/ci/.aircloak_ci/data/cache/ -printf '%u %p\n' |
  awk '{ if ($1 == "root") { print $2} }' |
  xargs -L 1 chown ci:ci
} || true
