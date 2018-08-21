#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})/../..

. docker/docker_helper.sh

build_aircloak_image ldap air/ldap/Dockerfile air/ldap/.dockerignore
