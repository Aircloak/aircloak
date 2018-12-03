#!/bin/bash

# Script for measuring aircloak performance on diffix0.mpi-sws.org

set -eo pipefail

function stop_container {
  docker rm -f $1 > /dev/null 2>&1 || true
}

function restart_performance_db {
  stop_container performance_db

  docker run \
    --detach --name performance_db -p 15432:5432 \
    --tmpfs=/ramdisk:rw \
    -e PGDATA=/ramdisk \
    quay.io/aircloak/performance_db:latest postgres -c config_file=/etc/postgresql/postgresql.conf \
    > /dev/null
}

function recreate_db {
  local num_users=$1

  restart_performance_db

  docker restart performance_cloak > /dev/null

  sleep 5

  docker exec -it performance_cloak /aircloak/cloak/bin/cloak rpc "
    Cloak.PerformanceData.generate(
      num_users: $num_users,
      hostname: \"diffix0.mpi-sws.org\",
      port: 15432,
      username: \"postgres\"
    )
  " > /dev/null
}

function performance {
  docker exec -it performance_air /aircloak/air/bin/air rpc "
    Air.Performance.run(
      Path.join(Application.app_dir(:air, \"priv\"), \"config\"),
      \"performance@aircloak.com\",
      \"1234\",
      exclude_encoded: true
    )
  "
}

if [ "$#" -lt 1 ]; then
  echo "usage ${BASH_SOURCE[0]} num_users1 num_users2 ..." >&2
  exit 1
fi

docker pull quay.io/aircloak/performance_db:latest > /dev/null

for num_users in "$@"; do
  recreate_db $num_users
  performance
done
