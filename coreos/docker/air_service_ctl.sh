#!/usr/bin/env bash

set -eo pipefail

AIR_ENV=prod /aircloak/air/$1/container.sh $2
