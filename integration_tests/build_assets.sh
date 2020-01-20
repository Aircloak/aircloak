#!/bin/bash

set -eo pipefail

cd /aircloak/air
make deps
cd /aircloak/air/assets
yarn deploy
