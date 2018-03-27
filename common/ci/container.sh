#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. docker/ci_helper.sh common

mount_to_aircloak VERSION

mount_to_component \
  elixir/config elixir/lib elixir/priv elixir/test elixir/mix.exs elixir/mix.lock elixir/Makefile \
  elixir/check_warnings.sh elixir/include elixir/.formatter.exs

mount_cached_component elixir/deps elixir/_build .bash_history

case "$1" in
  prepare_for_test)
    :
    ;;

  *)
    default_handle "$@"
    ;;
esac
