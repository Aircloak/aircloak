#!/usr/bin/env bash

set -eo pipefail

# Everything is enclosed into a block, which ensures that bash will load and
# parse the whole code. This allows the script to remove itself while uninstalling
# the system, and bash can still execute the code.
{
  . /etc/environment
  . /aircloak/docker/docker_helper.sh
  . /aircloak/air/config/config.sh

  # export air environment vars
  export $(cat /aircloak/air/environment | xargs)

  function stop_system {
    # Explicitly stop main services
    systemctl stop air-insights air-router air-static-site

    # Then stop the prerequisites service
    systemctl stop air-prerequisites

    cleanup_dead_containers
  }

  function cleanup_dead_containers {
    # Workaround for a nasty Docker bug where a dead container can be left dangling and can't be removed
    # with `docker rm` (https://github.com/docker/docker/issues/14474).
    # It has been shown experimentally that this can be resolved by removing the container folder,
    # then recreating an empty folder, and finally issuing docker stop and docker rm.
    # This is a known bug that is supposed to be fixed in Docker 1.9, but until then we're doing this
    # ugly hack.
    for dead_container_id in $(docker ps -a --filter 'status=dead' --format "{{.ID}}" --no-trunc); do
      echo "Cleaning up dead container $dead_container_id"
      rm -rf /var/lib/docker/overlay/$dead_container_id || true
      mkdir -p /var/lib/docker/overlay/$dead_container_id
      docker stop $dead_container_id || true
      docker rm $dead_container_id
    done
  }

  function check_system {
    wanted_services="air-insights air-prerequisites air-router air-static-site"
    active_services=$(
          systemctl list-units |
          grep 'air\-' |
          awk '{ if ($2 == "loaded" && $3 == "active") print $1}' |
          sed "s/.service//" |
          sort |
          tr '\n' ' ' |
          sed 's/\s*$//' || true
        )
    if [ "$active_services" != "$wanted_services" ]; then
      echo "Wanted services: $wanted_services"
      echo "Active services: $active_services"
      return 0
    fi

    router_code=$(http_code $(get_tcp_port prod router/http))
    if [ "$router_code" != "403" ]; then
      echo "router http request returned $router_code (expected 403)"
      return 0
    fi

    insights_code=$(http_code $(get_tcp_port prod insights/http))
    if [ "$insights_code" != "302" ]; then
      echo "insights http request returned $insights_code (expected 302)"
      return 0
    fi

    www_code=$(http_code 10000)
    if [ "$www_code" != "200" ]; then
      echo "www http request returned $www_code (expected 200)"
      return 0
    fi

    echo "ok"
  }

  function http_code {
    curl -s -w %{http_code} --output /dev/null http://127.0.0.1:$1 || true
  }

  function wait_until_system_is_up {
    attempt=1
    result=$(check_system)

    while [ "$result" != "ok" ]; do
      if [ "$attempt" == "60" ]; then
        echo "$result"
        exit 1
      fi

      sleep 1
      result=$(check_system)
      attempt=$(($attempt + 1))
    done

    echo "ok"
  }

  function upgrade_system {
    # We'll stop update services to avoid restart during an upgrade
    sudo systemctl stop locksmithd
    sudo systemctl stop update-engine

    # make sure cloud-config is good
    sudo coreos-cloudinit --validate --from-file=/var/lib/coreos-install/user_data

    # stop local services
    stop_system

    # remove old service files if they exist
    rm `ls -1 /etc/systemd/system/air-* | grep -v 'air-installer'` || true

    # remove air files
    rm -rf /aircloak/air

    # tail installation log
    follow_installation &

    # force reinstallation
    echo "Reinstalling system. This may take a while ..."
    sudo coreos-cloudinit --from-file=/var/lib/coreos-install/user_data

    # Make sure not to process until the installation has finished. In principle,
    # this shouldn't happen since restart is synchronous, but we do it just to be on the
    # safe side.
    while [ ! -e /aircloak/air/install/.installed ]; do
      echo "Installation not yet finished"
      sleep 2
    done

    cleanup_dead_containers

    echo 'Applying the new cloud-config...'
    sudo coreos-cloudinit --from-file=/var/lib/coreos-install/user_data
    echo 'Air system successfully installed!'

    # start update services again
    sudo systemctl start locksmithd
    sudo systemctl start update-engine
  }

  function remove_unused_images {
    all_images=$(docker images | awk '{print $1":"$2}' | tail -n +2 | sed "s/:latest\$//")
    used_images=$(docker ps --format "{{.Image}}")

    for image in $all_images; do
      if [[ ! "$used_images" =~ ^.*"$image".*$ ]]; then
        echo "Removing unused image $image"
        docker rmi "$image"
      fi
    done
  }

  function follow_installation {
    # tail installation log
    journalctl -f -n 200 -u air-installer&
    pid=$!

    while [ ! -e /aircloak/air/install/.installed ]; do sleep 2; done

    # kill background log process
    {
      kill -9 $pid && wait $pid 2>/dev/null
    } || true
  }

  function latest_local_version {
    echo "$(
          find_images "$1" "" version |
          grep -v latest |
          sort -t "." -k "1,1rn" -k "2,2rn" -k "3,3rn" |
          head -n 1 || true
        )"
  }



  case "$1" in
    start_service)
      shift
      image_name=$(/aircloak/air/$1/container.sh image_name)
      latest_local_version=$(latest_local_version $REGISTRY_URL/$image_name)
      DOCKER_IMAGE_VERSION="$latest_local_version" AIR_ENV=prod /aircloak/air/$1/container.sh foreground
      ;;

    stop_service)
      shift
      AIR_ENV=prod /aircloak/air/$1/container.sh stop
      ;;

    stop_system)
      stop_system
      ;;

    upgrade_system)
      upgrade_system
      ;;

    follow_installation)
      follow_installation
      ;;

    wait_until_system_is_up)
      wait_until_system_is_up
      ;;

    remove_unused_images)
      remove_unused_images
      ;;

    *)
      exit 1
      ;;
  esac

  exit 0
}
