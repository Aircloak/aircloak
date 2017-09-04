#!/usr/bin/env bash

set -eo pipefail

./air/production.sh attack-air deploy
./cloak/production.sh attack-cloak1 deploy
./cloak/production.sh attack-cloak2 deploy
./cloak/production.sh attack-cloak3 deploy
