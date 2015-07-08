#!/bin/bash

set -eo pipefail

function start_air_service {
  fleetctl destroy $1
  fleetctl load /aircloak/air/$1
  fleetctl start $1
}

start_air_service backend.service
start_air_service backend-discovery.service

start_air_service frontend.service
start_air_service frontend-discovery.service
