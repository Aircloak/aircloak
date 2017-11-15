#!/bin/bash

set -eo pipefail

# run from the top-level folder of the project
ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/.. && pwd)
cd $ROOT_DIR

. ci/cloak.sh


command=$1
shift || true

case "$command" in
  cloak_compliance)
    start_cloak_with_databases
    run_in_cloak "
      export DEFAULT_SAP_HANA_SCHEMA='$DEFAULT_SAP_HANA_SCHEMA' &&
      MIX_ENV=test mix gen.test_data dockerized_ci 10 &&
      mix test --only compliance --max-cases 4
    "
    ;;

  debug_cloak_compliance)
    start_cloak_with_databases
    run_in_cloak "
      MIX_ENV=test mix gen.test_data dockerized_ci 10 &&
      /bin/bash
    "
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
