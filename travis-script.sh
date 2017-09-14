#!/bin/bash

set -eo pipefail

function banner() {
  component=$1
  echo
  echo
  echo
  echo "# -------------------------------------------------------------------"
  echo "# Tests for: $component"
  echo "# -------------------------------------------------------------------"
  echo
}

# Sub-shell, so we don't change paths, and things get confusing
(
  # Source asdf, once and for all, so we are using the right
  # versions of Erlang, Elixir and NodeJS
  . ~/.asdf/asdf.sh


  # common/elixir -----------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    banner "common/elixir"
    pushd common/elixir
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # air ---------------------------------------------------------------

  if [[ "$TEST" == "air" ]]; then

    banner "air"
    pushd air
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # cloak -------------------------------------------------------------

  if [[ "$TEST" == "cloak" ]]; then

    banner "cloak"
    pushd cloak
    make docs
    make lint
    make odbc_drivers
    mix test --exclude compliance --include exclude_in_dev --max-cases 10
    make dialyze
    make proper-extended
    popd

  fi

  # compliance --------------------------------------------------------

  if [[ "$TEST" == "compliance" ]]; then

    banner "compliance"
    docker run --net host -v $(pwd):/aircloak aircloak/cloak_dev:latest aircloak/cloak/travis_compliance.sh

  fi

  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    banner "bom"
    pushd bom
    make docs
    make lint
    make test
    make dialyze
    mix bom --elixir ../cloak/deps --elixir ../air/deps --node ../air/node_modules ./
    popd

  fi


  # central -----------------------------------------------------------

  if [[ "$TEST" == "central" ]]; then

    banner "central"
    pushd central
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # integration_tests -----------------------------------------------------------

  if [[ "$TEST" == "integration" ]]; then

    banner "integration_tests"
    pushd integration_tests
    INTEGRATION_TEST=true mix test
    popd

  fi
)
