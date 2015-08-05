#!/bin/bash

set -eo pipefail

cd $(dirname $0)

if [ -z $COREOS_HOST_IP ]; then
  echo "
COREOS_HOST_IP not set. Please run with:

  COREOS_HOST_IP=x.y.z $0

where x.y.z is the IP of your host on your local network.
"
exit 1
fi

./create_user_data.sh

vagrant halt || true
vagrant up --provision
