#!/bin/bash

set -eox pipefail

# Sub-shell, so we don't change paths, and things get confusing
(
  # Source asdf, once and for all, so we are using the right
  # versions of Erlang, Elixir and NodeJS
  . ~/.asdf/asdf.sh


  # common/elixir -----------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    pushd common/elixir
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # air ---------------------------------------------------------------

  if [[ "$TEST" == "air" ]]; then

    pushd air
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # cloak -------------------------------------------------------------

  if [[ "$TEST" == "cloak" ]]; then

    pushd cloak
    make docs
    make lint
    make odbc_drivers
    mix test --include exclude_in_dev --max-cases 10
    make dialyze
    make proper-extended
    popd

  fi


  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    pushd bom
    make docs
    make lint
    make test
    make dialyze
    mix bom --elixir ../cloak/deps --elixir ../air/deps --node ../air/assets/node_modules ./
    popd

  fi


  # central -----------------------------------------------------------

  if [[ "$TEST" == "central" ]]; then

    pushd central
    make docs
    make lint
    make test
    make dialyze
    popd

  fi


  # integration_tests -----------------------------------------------------------

  if [[ "$TEST" == "integration" ]]; then

    pushd integration_tests
    INTEGRATION_TEST=true mix test
    popd

  fi
)

set +x
