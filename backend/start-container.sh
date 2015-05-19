#!/bin/bash

set -e

docker run --rm --name air -it -p 11000:11000 -p 9000:9000 "$@" aircloak/air:latest
