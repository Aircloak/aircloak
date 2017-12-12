#!/bin/bash

shift || true

command="$1"
shift || true

exec_on_prod() {
  $RELEASE_ROOT_DIR/bin/aircloak_ci eval "'Elixir.AircloakCI.ReleaseCLI':$1"
}

case "$command" in
  force_pr_build)
    exec_on_prod "force_pr_build($1)"
    ;;

  force_branch_build)
    exec_on_prod "force_branch_build($1)"
    ;;

  # backwards compatibility
  force_start)
    exec_on_prod "force_pr_build($1)"
    ;;

  log)
    exec_on_prod "print_build_log($1)"
    ;;

  *)
    echo "Commands:"
    echo ""
    echo "  force_pr_build pull_request_number"
    echo "  force_branch_build branch_name"
    echo "  log pull_request_number"
    echo ""
    exit 1
    ;;
esac
