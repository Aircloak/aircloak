#!/bin/bash

set -e

nginx_pid="$(ps augx | grep nginx\ -c | grep -v grep | awk '{print $2}')"
if [ -z "$nginx_pid" ]; then
  echo "[aircloak] Nginx not running, not stopping"
else
  echo "[aircloak] Nginx is already running, killing it"
  kill "$nginx_pid"
fi
