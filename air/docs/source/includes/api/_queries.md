## Queries

### Running a query

```ruby
RestClient.post "#{site_url}/api/queries",
  api_token,
  {
    query: {
      statement: statement,
      data_source_id: data_source_id
    }
  }.to_json,
  {"Content-Type" => "application/json"}
```

```shell
wget \
  --content-on-error \
  --output-document - \
  --method=POST \
  --header "auth-token: $API_TOKEN" \
  --header "Content-Type: application/json" \
  --body-data "{\"query\": {\"statement\": \"$statement\", \"data_source_id\": \"$data_source_id\"}}" \
  $SITE_URL/api/queries
```

```curl
curl -v \
  -X POST \
  -H "auth-token:$API_TOKEN" \
  -H "Content-Type:application/json" \
  -d "{\"query\": {\"statement\": \"$statement\", \"data_source_id\": \"$data_source_id\"}}" \
  $SITE_URL/api/queries
```

This endpoint starts a query asynchronously. You need to provide a valid data source id, which you can obtain with the [data source endpoint](#get-the-list-of-all-data-sources).

#### HTTP Request

`POST /api/queries`

#### Response

```json
{
  "success": true,
  "query_id": "<query-id>"
}
```

The API return value is the success information and the id of the query. You can use this id to [get the query result](#getting-the-result-of-a-query).


### Getting the result of a query

```ruby
RestClient.get "#{site_url}/api/queries/#{query_id}"
```

```shell
wget \
  --content-on-error \
  --output-document - \
  --method=GET \
  --header "auth-token: $API_TOKEN" \
  $SITE_URL/api/queries/$query_id
```

```curl
curl -v \
  -X GET \
  -H "auth-token:$API_TOKEN" \
  $SITE_URL/api/queries/$query_id
```

This endpoint returns the status of the query started by the [run query endpoint](#running-a-query). The `query_id` must correspond to the id returned by the run query endpoint.

#### HTTP Request

`GET /api/queries/query_id`

#### Response

```text
{
  "query": {
    "completed": true,
    "id": "<query-id>",
    "statement": "<query-statement>",
    "error": "<error-message>",
    "columns": ["<column-name>", ...],
    "row_count": <row-count>,
    "rows": [
      {"row": [<value-1>, ...], "occurrences": <number-of-occurrences>},
      ...
    ]
  }
}
```

The API return value contains information about the query.

- If the query is still running, the `completed` field will be set to false. In this case, the client needs to
repeat the request a bit later.
- If the query has completed, the `completed` field will be set to true.
  - If the query succeeded, the result can be obtained in fields `columns`, `row_count`, and `rows`.
  - If the query failed, the `error` field will contain a descriptive error message.

For the use of error codes in the Web REST API, please consult the [Errors](#errors) section.
