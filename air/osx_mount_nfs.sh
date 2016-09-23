#!/bin/bash

# Adapted from https://gist.github.com/olalonde/3f7512c0bd2bc8abb46d
#
# This script will mount given folders in the docker-machine VM using NFS (instead of the
# default vboxsf). It's probably not a good idea to run it while there are
# Docker containers running in docker-machine.
#
# Usage: sudo ./osx_mount_nfs.sh docker_machine_name my_machine_ip abs_path1 abs_path2 ...
#
# Note: You need to add the following line in the /etc/nfs.conf file:
#       nfs.server.mount.require_resv_port = 0

if [ "$USER" != "root" ]
then
  echo "This script must be run with sudo: sudo ${0}"
  exit -1
fi

# Run command as non root http://stackoverflow.com/a/10220200/96855
DOCKER_MACHINE_NAME=$1
OSX_IP=$2
if [ "$DOCKER_MACHINE_NAME" == "" ] || [ "$OSX_IP" == "" ]
then
  echo "Usage: $0 docker_machine_name my_machine_ip folder1 folder2"
  exit 1
fi

shift 2

MAP_USER=${SUDO_USER}
MAP_GROUP=$(sudo -u ${SUDO_USER} id -n -g)

for FOLDER in "$@"; do
  # Delete previously generated line if it exists
  $(mv /etc/exports /etc/exports.old)
  grep -v '^'$FOLDER' ' /etc/exports.old > /etc/exports
  rm /etc/exports.old

  echo ""$FOLDER" -mapall=${MAP_USER}:${MAP_GROUP} ${OSX_IP}" >> /etc/exports
done

nfsd restart

for FOLDER in "$@"; do
  sudo -u ${SUDO_USER} docker-machine ssh $DOCKER_MACHINE_NAME << EOF
    echo "Unmounting "$FOLDER""
    sudo umount $FOLDER 2> /dev/null
    exit
EOF
done

sudo -u ${SUDO_USER} docker-machine ssh $DOCKER_MACHINE_NAME << EOF
  echo "Restarting nfs-client"
  sudo /usr/local/etc/init.d/nfs-client restart
  echo "Waiting 10s for nfsd and nfs-client to restart."
  sleep 10
  exit
EOF

for FOLDER in "$@"; do
  sudo -u ${SUDO_USER} docker-machine ssh $DOCKER_MACHINE_NAME << EOF
    echo "Mounting "$FOLDER""
    sudo mkdir -p $FOLDER

    sudo mount $OSX_IP:$FOLDER $FOLDER -o rw,async,noatime,rsize=32768,wsize=32768,proto=tcp,nfsvers=3
    echo "Mounted "$FOLDER
    exit
EOF
done
