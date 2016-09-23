#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)

if [ $# -ne 1 ]; then
  echo
  echo "Usage:"
  echo "  $0 deploy_configuration"
  echo
fi

build_branch=$(git symbolic-ref --short HEAD)
if [ "$build_branch" != "master" ]; then
  echo "Warning: deploying from branch $build_branch"
  read -p "Continue (y/N)? " -r
  if ! [[ $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
fi

echo "Deploying cloak"
SKIP_BRANCH_CHECK=true ./cloak/production.sh $1 deploy

echo
echo "Deploying air"
SKIP_BRANCH_CHECK=true ./air/production.sh $1 deploy
