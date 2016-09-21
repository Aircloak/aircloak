#!/bin/bash -e

# prepare system
apt-get install -y libgss3
sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
locale-gen en_US.UTF-8

# download driver
wget https://download.microsoft.com/download/2/E/5/2E58F097-805C-4AB8-9FC6-71288AB4409D/msodbcsql-13.0.0.0.tar.gz

# setup driver
tar -xzf msodbcsql-13.0.0.0.tar.gz
cd msodbcsql-13.0.0.0
./install.sh install --accept-license --force

# cleanup
cd ..
rm msodbcsql-13.0.0.0.tar.gz
rm -rf msodbcsql-13.0.0.0
