#!/bin/bash -e

wget https://download.aircloak.com/.odbc/maprdrill_1.3.16-2_amd64.deb -nv -O odbc_drill_driver.deb
dpkg -i odbc_drill_driver.deb
rm odbc_drill_driver.deb
