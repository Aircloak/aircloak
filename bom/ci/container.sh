#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh bom

mount_to_aircloak VERSION common/elixir
mount_to_component config lib priv test mix.exs mix.lock Makefile .gitignore check_warnings.sh .formatter.exs
mount_cached_component deps _build .bash_history

case "$1" in
  prepare_for_test)
    :
    ;;

  *)
    default_handle "$@"
    ;;
esac
