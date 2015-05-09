#!/usr/bin/env bash

mkdir -p test/
cp -rp rel/files/sys.config test/

cat "rel/files/sys.config" | \
    sed 's,host\, "air_db",host\, "127.0.0.1",' | \
    sed 's,user\, "air",user\, "airtest",' | \
    sed 's,database\, "aircloakdatabase",database\, "air_test_database",' \
      > "test/sys.config"
