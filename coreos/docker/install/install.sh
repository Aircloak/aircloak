#!/usr/bin/env bash

set -eo pipefail

. /etc/environment
. /aircloak/air/common/docker_helper.sh

function pull_docker_image {
  latest_version=$(
        curl -s "$REGISTRY_URL/v2/$1/tags/list" |
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
pull_docker_image aircloak/air_router
pull_docker_image aircloak/air_backend
pull_docker_image aircloak/air_frontend

# Purge memory, because it seems to affect starting of Docker containers
sync && echo 3 > /proc/sys/vm/drop_caches
