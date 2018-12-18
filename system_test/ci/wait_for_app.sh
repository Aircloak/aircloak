#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

function app_running {
  local container=$1
  local app=$2

  running=$(elixir_rpc $container $app "Application.started_applications() |> Enum.any?(& elem(&1,0) == :$app)")
  if [ "$running" == "true" ]; then return 0; else return 1; fi
}

function elixir_rpc {
  local container=$1
  local app=$2
  shift 2 || true

  docker exec ${container}_${app} /aircloak/$app/bin/$app rpc "$@"
}

container=$1
app=$2

# wait for the beam process to start
while [ "$(docker exec ${container}_${app} ps aux | grep progname\ aircloak)" == "" ]; do sleep 1; done

# wait for the node to be available
while [ "$(docker exec ${container}_${app} /aircloak/$app/bin/$app ping)" != "pong" ]; do sleep 1; done

# wait for the app to be started
until $(app_running $container $app); do sleep 1; done
