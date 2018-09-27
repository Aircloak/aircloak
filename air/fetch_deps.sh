#!/usr/bin/env bash

set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR/air

function exec_with_retry {
  local attempts=10
  while [ $attempts -gt 0 ]; do
    if eval "$@"; then
      break
    else
      if [ $attempts -gt 1 ]; then sleep 1; fi
      let attempts=attempts-1
    fi
  done

  if [ $attempts -eq 0 ]; then exit 1; fi
}

# The retry logic is used because fetching deps might occasionally fail, especially with yarn packages.
exec_with_retry "mix deps.get $@"
exec_with_retry "cd $ROOT_DIR/air/assets && yarn install"
exec_with_retry "cd $ROOT_DIR/air/docs && yarn install"
exec_with_retry '
  cd $ROOT_DIR/air
  gitbook_version=$(cat docs/node_modules/gitbook/package.json | jq -r ".version")
  if [ ! -d ~/.gitbook/versions/$gitbook_version ]; then yarn run gitbook fetch $gitbook_version; fi
'
