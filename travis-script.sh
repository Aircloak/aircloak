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

  # compliance --------------------------------------------------------

  if [[ "$TEST" == "compliance" && "$TRAVIS_EVENT_TYPE" != "push" ]]; then

    docker run --net host \
      -v $(pwd):/aircloak \
      -e TRAVIS="$TRAVIS" \
      -e TRAVIS_BRANCH="$TRAVIS_BRANCH" \
      -e TRAVIS_EVENT_TYPE="$TRAVIS_EVENT_TYPE" \
      -e TEST="$TEST" \
      aircloak/cloak_dev:latest aircloak/cloak/travis_compliance.sh

  fi

  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

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

    pushd central
    rm -rf _build/dev/dialyze_erlang-20.1_elixir-1.5.1_deps-dev.plt || true
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
