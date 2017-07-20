#!/bin/bash -e

# prepare system
apt-get install -y libgss3 apt-transport-https
sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
locale-gen en_US.UTF-8

# setup apt sources
curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
curl https://packages.microsoft.com/config/debian/8/prod.list > /etc/apt/sources.list.d/mssql-release.list
apt-get update

# setup driver
ACCEPT_EULA=Y apt-get install -y unixodbc msodbcsql
