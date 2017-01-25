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

  if [[ "$TEST" == "common" ]]; then

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
    make test_all
    make dialyze
    make proper-extended
    popd

  fi


  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "common" ]]; then

    banner "bom"
    pushd bom
    make docs
    make lint
    make test
    make dialyze
    mix bom --elixir ../cloak/deps --elixir ../air/deps --node ../air/node_modules bom.json
    popd

  fi


  # central -----------------------------------------------------------

  if [[ "$TEST" == "common" ]]; then

    banner "central"
    pushd central
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # integration_tests -----------------------------------------------------------

  if [[ "$TEST" == "common" ]]; then

    banner "integration_tests"
    pushd integration_tests
    INTEGRATION_TEST=true mix test
    popd

  fi
)
