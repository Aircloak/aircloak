#!/usr/bin/env bash

set -eo pipefail

./air/production.sh demo-air deploy
./cloak/production.sh demo-cloak deploy
