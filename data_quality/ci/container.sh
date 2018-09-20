#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh data_quality

mount_to_aircloak VERSION common/elixir
mount_to_component .formatter.exs check_warnings.sh config lib Makefile mix.exs mix.lock setup.r test beta.r
mount_cached_component deps _build

case "$1" in
  prepare_for_test)
    :
    ;;

  *)
    default_handle "$@"
    ;;
esac
