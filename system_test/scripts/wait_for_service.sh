#!/bin/bash

set -eo pipefail

until $(curl --output /dev/null --silent --head --fail http://$1); do
  sleep 5
done
