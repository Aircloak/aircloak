#!/bin/bash

# Adapted from https://gist.github.com/olalonde/3f7512c0bd2bc8abb46d
#
# This script will mount given folders in the boot2docker VM using NFS (instead of the
# default vboxsf). It's probably not a good idea to run it while there are
# Docker containers running in boot2docker.
#
# Usage: sudo ./osx_mount_nfs.sh abs_path1 abs_path2 ...
#
# Note: You need to add the following line in the /etc/nfs.conf file:
#       nfs.server.mount.require_resv_port = 0

if [ "$USER" != "root" ]
then
  echo "This script must be run with sudo: sudo ${0}"
  exit -1
fi

# Run command as non root http://stackoverflow.com/a/10220200/96855
B2D_IP=$(sudo -u ${SUDO_USER} boot2docker ip)

if [ "$?" != "0" ]
then
  sudo -u ${SUDO_USER} boot2docker up
  $(sudo -u ${SUDO_USER} boot2docker shellinit)
  B2D_IP=$(sudo -u ${SUDO_USER} boot2docker ip)
fi

MAP_USER=${SUDO_USER}
MAP_GROUP=$(sudo -u ${SUDO_USER} id -n -g)

for FOLDER in "$@"; do
  # Delete previously generated line if it exists
  $(mv /etc/exports /etc/exports.old)
  grep -v '^'$FOLDER' ' /etc/exports.old > /etc/exports
  rm /etc/exports.old

  echo ""$FOLDER" -mapall=${MAP_USER}:${MAP_GROUP} ${B2D_IP}" >> /etc/exports
done

nfsd restart

for FOLDER in "$@"; do
  sudo -u ${SUDO_USER} boot2docker ssh << EOF
    echo "Unmounting "$FOLDER""
    sudo umount $FOLDER 2> /dev/null
    exit
EOF
done

sudo -u ${SUDO_USER} boot2docker ssh << EOF
  echo "Restarting nfs-client"
  sudo /usr/local/etc/init.d/nfs-client restart
  echo "Waiting 10s for nfsd and nfs-client to restart."
  sleep 10
  exit
EOF

for FOLDER in "$@"; do
  sudo -u ${SUDO_USER} boot2docker ssh << EOF
    echo "Mounting "$FOLDER""
    sudo mkdir -p $FOLDER

    # 192.168.59.3 is the address of the host as seen from boot2docker machine
    sudo mount 192.168.59.3:$FOLDER $FOLDER -o rw,async,noatime,rsize=32768,wsize=32768,proto=tcp,nfsvers=3
    echo "Mounted "$FOLDER
    exit
EOF
done
