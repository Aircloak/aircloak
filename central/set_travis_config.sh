#!/usr/bin/env bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
travis_config=$(cat priv/config/test.json | sed s/20005/5432/)
echo "$travis_config" > priv/config/test.json
