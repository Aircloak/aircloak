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

  # setup database roles ----------------------------------------------

  if [[ "$TEST" == "air" || "$TEST" == "integration" ]]; then

    banner "air DB roles"

    psql -U postgres -c "CREATE USER airtest CREATEDB;"
    psql -U postgres -c "CREATE DATABASE air_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE air_test TO airtest;"
    psql -U postgres -c "ALTER DATABASE air_test OWNER TO airtest;"

  fi

  if [[ "$TEST" == "central" || "$TEST" == "integration" ]]; then

    banner "central DB roles"

    psql -U postgres -c "CREATE USER central_test CREATEDB;"
    psql -U postgres -c "CREATE DATABASE central_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE central_test TO central_test;"
    psql -U postgres -c "ALTER DATABASE central_test OWNER TO central_test;"

  fi


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

  if [[ "$TEST" == "air" || "$TEST" == "aux" ]]; then

    banner "air deps"
    pushd air
    make deps
    popd

  fi

  if [[ "$TEST" == "air" ]]; then

    banner "air"
    pushd air

    mix compile --warnings-as-errors
    MIX_ENV=test mix compile --warnings-as-errors
    MIX_ENV=prod mix compile --warnings-as-errors
    MIX_ENV=test make recreate-db
    popd

  fi


  # cloak -------------------------------------------------------------

  if [[ "$TEST" == "cloak" || "$TEST" == "aux" || "$TEST" == "compliance" ]]; then

    banner "cloak deps"
    pushd cloak
    make deps
    popd

  fi

  if [[ "$TEST" == "cloak" ]]; then

    banner "cloak"
    pushd cloak
    mix compile --warnings-as-errors
    mix config_sap_hana_test_schema
    mix gen.test_data "compliance" 200
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

  if [[ "$TEST" == "central" ]]; then

    banner "central"
    pushd central

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
    # Start Air to let it migrate the DB
    MIX_ENV=test mix run --no-start -e 'Application.ensure_all_started(:air)'
    popd

  fi
)
