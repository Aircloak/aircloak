#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

function app_running {
  local container=$1
  local app=$2

  local running=$(erlang_eval $container $app "[App || {App, _, _} <- application:which_applications(), App =:= $app]")
  if [ "$running" == "[]" ]; then return 1; else return 0; fi
}

function erlang_eval {
  local container=$1
  local app=$2
  shift 2 || true

  docker exec ${container}_${app} /aircloak/$app/bin/$app eval $@
}

container=$1
app=$2

while [ "$(docker exec ${container}_${app} /aircloak/$app/bin/$app ping)" != "pong" ]; do sleep 1; done
until $(app_running $container $app); do sleep 1; done
