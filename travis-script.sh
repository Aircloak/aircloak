#!/bin/bash

set -eox pipefail

# Sub-shell, so we don't change paths, and things get confusing
(
  # Source asdf, once and for all, so we are using the right
  # versions of Erlang, Elixir and NodeJS
  . ~/.asdf/asdf.sh


  # central -----------------------------------------------------------

  if [[ "$TEST" == "central" ]]; then

    pushd central
    make docs
    make lint
    make test
    make dialyze
    popd

  fi
)

set +x
