#!/bin/bash

command="$1"
shift || true

exec_on_prod() {
  $RELEASE_ROOT_DIR/bin/aircloak_ci eval "'Elixir.AircloakCI.ReleaseCLI':$1"
}

case "$command" in
  force_build)
    exec_on_prod "force_build(<<\"$1\">>, <<\"$2\">>, <<\"$3\">>)"
    ;;

  # backwards compatibility
  force_start)
    exec_on_prod "force_build(<<\"pr\">>, <<\"$1\">>, <<\"compliance\">>)"
    ;;

  log)
    exec_on_prod "print_build_log(<<\"$1\">>, <<\"$2\">>, <<\"$3\">>)"
    ;;

  *)
    echo "Commands:"
    echo ""
    echo "  force_build target_type target_id job_name"
    echo "  log target_type target_id job_name"
    echo ""
    exit 1
    ;;
esac
