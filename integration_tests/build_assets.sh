#!/bin/bash

set -eo pipefail

cd /aircloak/air/assets
yarn install

cd /aircloak/air
assets/node_modules/brunch/bin/brunch build assets/
