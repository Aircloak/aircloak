#!/bin/bash

set -eo pipefail

apt-get install -y unixodbc odbc-postgresql libmyodbc libaio1

host_ip=$(getent hosts docker.for.mac.localhost | awk '{ print $1 }')
echo "$host_ip insights.air-local" >> /etc/hosts
cp /aircloak/cloak/priv/odbc/docker/odbc.ini /etc/

cp -rp /aircloak/cloak/deps/odbc_drivers/libodbc-sap-hana-v2.so /usr/lib/x86_64-linux-gnu/odbc/

cd /aircloak/cloak
bash
