#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh ci

mount_to_aircloak VERSION common/elixir
mount_to_component .gitignore config lib rel test mix.exs mix.lock Makefile README.md check_warnings.sh
mount_cached_component deps _build .bash_history

case "$1" in
  prepare_for_test)
    ;;

  *)
    default_handle "$@"
    ;;
esac
