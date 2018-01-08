#!/bin/bash

set -eox pipefail

# Sub-shell, so we don't change paths, and things get confusing
(
  function with_retries() {
    local retry=0
    local max_retries=5
    local interval=1 # second

    while [ ${retry} -lt ${max_retries} ]
    do
      "$@" && return 0
      retry=$[${retry}+1]
      echo "Retrying [${retry}/${max_retries}] in ${interval}(s) "
      sleep ${interval}
    done

    return 1
  }

  # Source asdf, once and for all, so we are using the right
  # versions of Erlang, Elixir and NodeJS
  . ~/.asdf/asdf.sh

  # setup database roles ----------------------------------------------

  if [[ "$TEST" == "air" || "$TEST" == "integration" ]]; then

    psql -U postgres -c "CREATE USER airtest CREATEDB;"
    psql -U postgres -c "CREATE DATABASE air_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE air_test TO airtest;"
    psql -U postgres -c "ALTER DATABASE air_test OWNER TO airtest;"

  fi

  if [[ "$TEST" == "central" || "$TEST" == "integration" ]]; then

    psql -U postgres -c "CREATE USER central_test CREATEDB;"
    psql -U postgres -c "CREATE DATABASE central_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE central_test TO central_test;"
    psql -U postgres -c "ALTER DATABASE central_test OWNER TO central_test;"

  fi


  # common/elixir -----------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    # common/elixir
    pushd common/elixir
    mix deps.get
    mix compile --warnings-as-errors
    MIX_ENV=test make all
    popd

  fi


  # air ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    pushd air
    make deps
    popd

  fi


  # cloak -------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    pushd cloak
    make odbc_drivers
    make deps
    popd

  fi


  # bom ---------------------------------------------------------------

  if [[ "$TEST" == "aux" ]]; then

    pushd bom
    make deps
    mix compile --warnings-as-errors
    popd

  fi


  # central -----------------------------------------------------------

  if [[ "$TEST" == "central" ]]; then

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

    pushd integration_tests
    MIX_ENV=test mix deps.get
    MIX_ENV=test mix compile
    # Start Air to let it migrate the DB
    MIX_ENV=test mix run --no-start -e 'Application.ensure_all_started(:air)'
    popd

  fi
)

set +x
