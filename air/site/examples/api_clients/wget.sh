#!/usr/bin/env bash

SITE_URL="https://insights.air-local:20000"

if [ "$API_TOKEN" == "" ]; then
  echo "Please export API_TOKEN"
  exit 1
fi

# fetching data sources
data_sources=$(wget \
  --content-on-error \
  --output-document - \
  --method=GET \
  --header "auth-token:$API_TOKEN" \
  --no-check-certificate \
  $SITE_URL/api/data_sources
)

# running a query
data_source_token=$(
  echo "$data_sources" |
    jq --raw-output "select(.[].id == \"local\")[0].token"
)

statement="SELECT itemname, count(*) FROM purchases GROUP by itemname"
run_query_response=$(
  wget \
    --content-on-error \
    --output-document - \
    --method=POST \
    --header "auth-token: $API_TOKEN" \
    --header "Content-Type: application/json" \
    --body-data "{\"query\": {\"statement\": \"$statement\", \"data_source_token\": \"$data_source_token\"}}" \
    --no-check-certificate \
    $SITE_URL/api/queries
)

# getting the query result
query_id=$(echo "$run_query_response" | jq --raw-output ".query_id")
query_result=$(
  wget \
    --content-on-error \
    --output-document - \
    --method=GET \
    --header "auth-token: $API_TOKEN" \
    --no-check-certificate \
    $SITE_URL/api/queries/$query_id
)

echo "$query_result"
