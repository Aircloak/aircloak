#!/bin/bash

# This script is intended for macos users to start a Linux container and run the cloak from there.
# The script will mount the local folder to the container, so local editing will work as expected.
# This is mostly intended for those rare scenarios where something needs to be tested on Linux.

set -eo pipefail

../common/docker/elixir/build-image.sh

aircloak_folder=$(cd .. && pwd)

docker run --rm -it \
  -v $aircloak_folder:/aircloak \
  aircloak/elixir:$(cat ../.tool-versions | grep elixir | awk '{print $2}') \
  /aircloak/cloak/dev-container-start-script.sh
