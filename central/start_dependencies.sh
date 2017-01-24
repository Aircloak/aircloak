#!/bin/bash

set -eo pipefail

cd $(dirname ${BASH_SOURCE[0]})
. ../docker/docker_helper.sh

./db/build-image.sh

echo "Starting databases for central"
DB_ENV="dev" ./db/container.sh ensure_started
DB_ENV="test" ./db/container.sh ensure_started

if ! named_container_running aircloak_elasticsearch ; then
  printf "Starting elasticsearch "
  docker run -d --name aircloak_elasticsearch -p 9200:9200 elasticsearch:5.0.0 > /dev/null

  until $(curl --output /dev/null --silent --head --fail http://localhost:9200); do
    printf '.'
    sleep 1
  done
  printf "\n"

  # initialize default index pattern for kibana
  curl -XPUT http://localhost:9200/.kibana/index-pattern/customer --silent --output /dev/null \
    -d '{"title" : "customer",  "timeFieldName": "timestamp"}'
  curl -XPUT http://localhost:9200/.kibana/config/5.0.0 --silent --output /dev/null -d '{"defaultIndex" : "customer"}'
fi
echo "elasticsearch running"

if ! named_container_running aircloak_kibana ; then
  printf "Starting kibana (this may take a few minutes) "
  docker run -d --name aircloak_kibana \
    --link aircloak_elasticsearch:elasticsearch \
    -p 5601:5601 \
    -v $(pwd)/kibana.yml:/etc/kibana/kibana.yml kibana:5.0.0 \
    > /dev/null

  until $(curl --output /dev/null --silent --head --fail http://localhost:5601); do
    printf '.'
    sleep 1
  done
  printf "\n"
fi
echo "kibana running"
