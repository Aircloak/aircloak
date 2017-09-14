#!/bin/bash

set -eo pipefail

cd aircloak/cloak

. $HOME/.asdf/asdf.sh
odbcinst -i -s -h -f priv/odbc/odbc_travis.ini
cp priv/config/travis_compliance.json priv/config/travis.json

TEST_SIZE=10
CONCURRENCY=10
mix gen.test_data compliance $TEST_SIZE
mix test --only compliance --max-cases $CONCURRENCY
