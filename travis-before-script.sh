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

  # These folders might be left over in the Travis cache. If they're present,
  # the BOM task will fail, since it picks up all dependecies in the deps folder.
  # Since gproc is no longer used, we can safely remove these folders from the
  # cache.
  rm -rf cloak/deps/gproc || true
  rm -rf air/deps/gproc || true
  rm -rf bom/deps/gproc || true

  # common/elixir -----------------------------------------------------

  banner "common/elixir"
  # common/elixir
  pushd common/elixir
  mix deps.get
  mix compile --warnings-as-errors
  MIX_ENV=test make all
  popd


  # air ---------------------------------------------------------------

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
  MIX_ENV=test make recreate_db
  popd


  # cloak -------------------------------------------------------------

  banner "cloak"
  pushd cloak
  make deps
  mix compile --warnings-as-errors
  MIX_ENV=test make all
  popd


  # bom ---------------------------------------------------------------

  banner "bom"
  pushd bom
  make deps
  mix compile --warnings-as-errors
  popd


  # central -----------------------------------------------------------

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
  MIX_ENV=test make recreate_db
  popd


  # integration_tests ---------------------------------------------------------------

  banner "integration_tests"
  pushd integration_tests
  MIX_ENV=test mix deps.get
  MIX_ENV=test mix compile
  popd
)
