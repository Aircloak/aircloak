#!/bin/bash

set -eo pipefail

function banner() {
  component=$1
  echo
  echo
  echo
  echo "# -------------------------------------------------------------------"
  echo "# Before-script: $component"
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
    # common/elixir
    pushd common/elixir
    mix deps.get
    mix compile --warnings-as-errors
    MIX_ENV=test make all
    popd

  fi


  # air ---------------------------------------------------------------

  if [[ "$TEST" == "air" ]]; then

    banner "air"
    pushd air

    psql -U postgres -c "CREATE USER airtest CREATEDB;"
    psql -U postgres -c "CREATE DATABASE air_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE air_test TO airtest;"
    psql -U postgres -c "ALTER DATABASE air_test OWNER TO airtest;"

    make deps
    mix compile --warnings-as-errors
    MIX_ENV=test mix compile --warnings-as-errors
    MIX_ENV=prod mix compile --warnings-as-errors
    MIX_ENV=test make recreate-db
    popd

  fi


  # cloak -------------------------------------------------------------

  if [[ "$TEST" == "cloak" ]]; then

    banner "cloak"
    pushd cloak
    make deps
    mix compile --warnings-as-errors
    MIX_ENV=test make all
    popd

  fi


  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    banner "bom"
    pushd bom
    make deps
    mix compile --warnings-as-errors
    popd

  fi


  # central -----------------------------------------------------------

  if [[ "$TEST" == "central" || "$TEST" == "integration" ]]; then

    banner "central"
    pushd central

    psql -U postgres -c "CREATE USER central_test CREATEDB;"
    psql -U postgres -c "CREATE DATABASE central_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE central_test TO central_test;"
    psql -U postgres -c "ALTER DATABASE central_test OWNER TO central_test;"

    make deps
    mix compile --warnings-as-errors
    MIX_ENV=test mix compile --warnings-as-errors
    MIX_ENV=prod mix compile --warnings-as-errors
    MIX_ENV=test make recreate-db
    popd

  fi


  # integration_tests ---------------------------------------------------------------

  if [[ "$TEST" == "integration" ]]; then

    banner "integration_tests"
    pushd integration_tests
    MIX_ENV=test mix deps.get
    MIX_ENV=test mix compile
    popd

  fi
)
