#!/bin/bash

set -eo pipefail

cd $(dirname $0)

air/start_dependencies.sh
central/start_dependencies.sh
