#!/usr/bin/env bash

set -eo pipefail

# Everything is enclosed into a block, which ensures that bash will load and
# parse the whole code. This allows the script to remove itself while uninstalling
# the system, and bash can still execute the code.
{
  . /etc/environment
  . /aircloak/air/common/docker_helper.sh

  # export air environment vars
  export $(cat /aircloak/air/environment | xargs)

  function stop_system {
    systemctl stop air-prerequisites
  }

  function check_system {
    active_services=$(
          systemctl list-units |
          grep 'air\-' |
          awk '{ if ($2 == "loaded" && $3 == "active") print $1}' |
          sed "s/.service//" |
          sort |
          tr '\n' ' ' |
          sed 's/\s*$//' || true
        )
    if
      [ "$active_services" == "air-backend air-frontend air-frontend-sidekick air-installer air-prerequisites air-router" ] &&
      [ "$(http_code $(get_tcp_port prod router/http))" == "403" ] &&
      [ "$(http_code $(get_tcp_port prod air_frontend/http))" == "302" ] &&
      [ "$(http_code $(get_tcp_port prod air_backend/http))" == "404" ]
    then
      return 0
    else
      return 1
    fi
  }

  function http_code {
    curl -s -w %{http_code} --output /dev/null http://127.0.0.1:$1 || true
  }

  function wait_until_system_is_up {
    while ! check_system; do
      sleep 1
    done
  }

  function upgrade_system {
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
    systemctl restart air-installer.service

    # Make sure not to process until the installation has finished. In principle,
    # this shouldn't happen since restart is synchronous, but we do it just to be on the
    # safe side.
    while [ ! -e /aircloak/air/install/.installed ]; do
      echo "Installation not yet finished"
      sleep 2
    done

    echo "Waiting for services to start ..."

    # Invoke the new version of the script
    /aircloak/air/air_service_ctl.sh wait_until_system_is_up
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


  case "$1" in
    start_service)
      shift
      AIR_ENV=prod /aircloak/air/$1/container.sh foreground
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

    *)
      exit 1
      ;;
  esac

  exit 0
}