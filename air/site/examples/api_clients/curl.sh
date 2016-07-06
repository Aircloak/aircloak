#!/usr/bin/env bash

if [ "$API_TOKEN" == "" ]; then
  echo "Please export API_TOKEN"
  exit 1
fi

SITE_URL="https://insights.air-local:20000"

# fetching data sources
data_sources=$(
  curl -v \
    -X GET \
    -H "auth-token:$API_TOKEN" \
    $SITE_URL/api/data_sources
)

# running a query
data_source_token=$(
  echo "$data_sources" |
    jq --raw-output "select(.[].id == \"local\")[0].token"
)

statement="SELECT itemname, count(*) FROM purchases GROUP by itemname"
run_query_response=$(
  curl -v \
    -X POST \
    -H "auth-token:$API_TOKEN" \
    -H "Content-Type:application/json" \
    -d "{\"query\": {\"statement\": \"$statement\", \"data_source_token\": \"$data_source_token\"}}" \
    $SITE_URL/api/queries
)

# getting the query result
query_id=$(echo "$run_query_response" | jq --raw-output ".query_id")
query_result=$(
  curl -v \
    -X GET \
    -H "auth-token:$API_TOKEN" \
    $SITE_URL/api/queries/$query_id
)

echo "$query_result"
