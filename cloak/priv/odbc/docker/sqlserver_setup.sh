#!/bin/bash -e

# change to script's path folder
cd "$(dirname "$0")"

# prepare system
apt-get install -y libgss3
sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
locale-gen en_US.UTF-8

# setup driver
tar -xzf msodbcsql-13.0.0.0.tar.gz
cd msodbcsql-13.0.0.0
./install.sh install --accept-license --force

# cleanup
cd ..
rm msodbcsql-13.0.0.0.tar.gz
rm -rf msodbcsql-13.0.0.0
