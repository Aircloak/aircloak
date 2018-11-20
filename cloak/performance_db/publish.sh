#!/usr/bin/env bash

set -eo pipefail

cd $(dirname $0)

version=$(cat VERSION)
quay="quay.io/aircloak/performance_db"

echo "NOTE: This will build the container locally and publish the result to quay as version:"
echo "      $quay:$version and $quay:latest"
echo

read -p "Continue? (y|N)  " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
  ./build-image.sh

  docker tag aircloak/performance_db:latest "$quay:latest"
  docker push "$quay:latest"

  docker tag aircloak/performance_db:latest "$quay:$version"
  docker push "$quay:$version"
fi
