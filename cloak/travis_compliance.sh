#!/bin/bash

set -eox pipefail

echo $TRAVIS
echo $TRAVIS_BRANCH
echo $TRAVIS_EVENT_TYPE

cd aircloak/cloak

. $HOME/.asdf/asdf.sh
make odbc_drivers
odbcinst -i -s -h -f priv/odbc/odbc_travis.ini
cp priv/config/travis_compliance.json priv/config/travis.json

TEST_SIZE=10
CONCURRENCY=10
mix config_sap_hana_test_schema
mix gen.test_data compliance $TEST_SIZE
mix test --only compliance --max-cases $CONCURRENCY
