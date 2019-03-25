#!/bin/bash

set -eo pipefail

cd /aircloak/air
make deps
assets/node_modules/brunch/bin/brunch build assets/
