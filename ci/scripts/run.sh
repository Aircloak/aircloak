#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

. ci/scripts/cloak.sh


command=$1
shift || true

case "$command" in
  build_cloak)
    build_cloak_image
    ;;

  run_in_cloak_test)
    run_in_cloak_test "$@"
    ;;

  cloak_compliance)
    cloak_compliance
    ;;

  debug_cloak_compliance)
    debug_cloak_compliance
    ;;
  *)
    commands="
      cloak_compliance - runs the compliance test suite
      debug_cloak_compliance - starts the interactive docker container which can be used for debugging
    "

    printf "\nUsage: $0 command\n\n"
    printf "Commands:\n\n"
    while read line; do
      if [ "$line" != "" ]; then
        echo $line | awk -F ' - ' '{printf "  %-20s%s\n", $1, $2}'
      fi
    done < <(echo "$commands" | sort)
    echo
    exit 1
    ;;
esac
