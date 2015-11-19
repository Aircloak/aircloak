#!/usr/bin/env bash

# This is the installer script that runs on the CoreOS machine on first boot.
# The installer pulls required images, populates local etcd instance, and sets
# up local systemd services.

set -eo pipefail

. /etc/environment
. /aircloak/air/common/docker_helper.sh

function pull_docker_image {
  latest_version=$(
        /aircloak/registry_v2_req $1/tags/list |
        jq --raw-output ".tags | select(. != null) | .[]" |
        sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
        head -n 1
      )

  if [ "$latest_version" == "" ]; then
    echo "Can't find any version of $1 in $REGISTRY_URL"
    exit 1
  fi

  echo "$(hostname): pulling image $REGISTRY_URL/$1:$latest_version ..."

  # This monstrosity takes care of some inexplicable timeouts that occur while pulling from
  # the docker registry. Here, we simply retry until pulling succeeds. Since pulling
  # is layered, the next attempt will resume where the last one stopped.
  until docker pull $REGISTRY_URL/$1:$latest_version; do
    sleep 1
  done

  echo "$(hostname): pulled image $REGISTRY_URL/$1:$latest_version"
}

function configure_etcd {
  echo "Waiting for etcd settings overrides ..."
  while [ ! -e /aircloak/etcd/coreos ]; do sleep 1; done

  # Copy settings to appropriate place
  mkdir -p /aircloak/air/etcd/local_settings
  cp -rp /aircloak/etcd/coreos /aircloak/air/etcd/local_settings/

  /aircloak/air/etcd/config_coreos.sh
}


# Setup common environment
cat /etc/environment >> /aircloak/air/environment
echo "REGISTRY_URL=$REGISTRY_URL" >> /aircloak/air/environment
echo "AIR_HOST_NAME=$COREOS_PUBLIC_IPV4" >> /aircloak/air/environment
echo "ETCD_CLIENT=http://127.0.0.1:$(get_tcp_port prod etcd/client)" >> /aircloak/air/environment

configure_etcd

touch /aircloak/air/.installation_started

# Pull images
pull_docker_image aircloak/static_website
pull_docker_image aircloak/air_router
pull_docker_image aircloak/air_backend
pull_docker_image aircloak/air_frontend

# Purge memory, because it seems to affect starting of Docker containers
sync && echo 3 > /proc/sys/vm/drop_caches

# Copy service files
for service in air-prerequisites air-router air-backend air-frontend air-frontend-sidekick air-static-site; do
  cp -rp /aircloak/air/$service.service /etc/systemd/system/
done


# Get the initial cloud config contents without the installer service
initial_cloud_config_without_installer=$(
      cat /var/lib/coreos-install/user_data |
      sed '/- name: air-installer.service/q' |
      sed '$d'
    )

# Get installer service contents, omitting the `command` directive, which effectively makes this service
# disabled. In other words, we'll keep the service but won't start it automatically again.
# The main reason for this is that we're reinitializing the cloud below. If this service was designated to be
# started, then we'll end up in a deadlock, with installer service trying to start itself. Thus, we
# simply disable the service, but still keep it in cloud config, so we can use it for subsequent updates.
disabled_installer=$(
      cat /var/lib/coreos-install/user_data |
      sed -n '/- name: air-installer.service/,$p' |
      grep -v 'command: start'
    )

final_cloud_config=$(
  cat <<EOF
$initial_cloud_config_without_installer
$disabled_installer

coreos:
  units:
  - name: air-static-site.service
    command: start

  - name: air-prerequisites.service
    command: start

  - name: air-router.service
    command: start

  - name: air-backend.service
    command: start

  - name: air-frontend.service
    command: start

  - name: air-frontend-sidekick.service
    command: start
EOF
)

echo "$final_cloud_config" > /var/lib/coreos-install/user_data
echo "Images retrieved, and new cloud-config generated."
touch /aircloak/air/install/.installed
