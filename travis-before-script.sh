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

  if [[ "$TEST" == "central" ]]; then

    psql -U postgres -c "CREATE USER central_test CREATEDB;"
    psql -U postgres -c "CREATE DATABASE central_test ENCODING 'UTF8';"
    psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE central_test TO central_test;"
    psql -U postgres -c "ALTER DATABASE central_test OWNER TO central_test;"

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
)

set +x
