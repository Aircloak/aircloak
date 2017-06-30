## Queries

### Running a query

{% codetabs name="Ruby", type="rb" -%}
payload = {
  query: {
    statement: statement,
    data_source_name: data_source_name
  }
}.to_json
content_type = {"Content-Type" => "application/json"}
RestClient.post("#{site_url}/api/queries", api_token, payload, content_type)


{%- language name="wget", type="sh" -%}
wget \
  --content-on-error \
  --output-document - \
  --method=POST \
  --header "auth-token: $API_TOKEN" \
  --header "Content-Type: application/json" \
  --body-data "{\"query\": {\"statement\": \"$statement\", \"data_source_name\": \"$data_source_name\"}}" \
  $SITE_URL/api/queries


{%- language name="curl", type="sh" -%}
curl -v \
  -X POST \
  -H "auth-token:$API_TOKEN" \
  -H "Content-Type:application/json" \
  -d "{\"query\": {\"statement\": \"$statement\", \"data_source_name\": \"$data_source_name\"}}" \
  $SITE_URL/api/queries
{%- endcodetabs %}

This endpoint starts a query asynchronously. You need to provide a valid data source name, which you can obtain with the [data source endpoint](data_sources.md#get-the-list-of-all-data-sources).


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

{% codetabs name="Ruby", type="rb" -%}
RestClient.get("#{site_url}/api/queries/#{query_id}", api_token)


{%- language name="wget", type="sh" -%}
wget \
  --content-on-error \
  --output-document - \
  --method=GET \
  --header "auth-token: $API_TOKEN" \
  $SITE_URL/api/queries/$query_id


{%- language name="curl", type="sh" -%}
curl -v \
  -X GET \
  -H "auth-token:$API_TOKEN" \
  $SITE_URL/api/queries/$query_id
{%- endcodetabs %}


This endpoint returns the status of the query started by the [run query endpoint](#running-a-query). The `query_id` must correspond to the id returned by the run query endpoint.


#### HTTP Request

`GET /api/queries/query_id`

#### Response

```json
{
  "query": {
    "completed": true,
    "query_state": "<the execution phase of the query>",
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
- The `query_state` parameter indicates how far along the query execution has come.
It can be one of:
  - `started` - The query has been scheduled, but has not yet started executing
  - `parsing` - The query syntax is being validated
  - `compiling` - The query semantics is validated and transformed into an internal form for execution
  - `awaiting_data` - The Cloak has made a database request for data, but has not yet received any data to process
  - `processing` - The Cloak is receiving data and is aggregating and anonymizing it
  - `completed` - The query execution has completed. The `completed` field will also be set to true in this state.
  - `error` - The query execution failed. The `completed` field will also be set to true in this state.
  - `cancelled` - The query was cancelled by a user. The `completed` field will also be set to true in this state.
- If the query has completed, the `completed` field will be set to true.
  - If the query succeeded, the result can be obtained in fields `columns`, `row_count`, and `rows`.
  - If the query failed, the `error` field will contain a descriptive error message.

For the use of error codes in the Web REST API, please consult the [Errors](#errors) section.


### Canceling a query

{% codetabs name="Ruby", type="rb" -%}
RestClient.post("#{site_url}/api/queries/#{query_id}/cancel", api_token, "")


{%- language name="wget", type="sh" -%}
wget \
  --content-on-error \
  --output-document - \
  --method=POST \
  --header "auth-token: $API_TOKEN" \
  $SITE_URL/api/queries/$query_id/cancel


{%- language name="curl", type="sh" -%}
curl -v \
  -X POST \
  -H "auth-token:$API_TOKEN" \
  $SITE_URL/api/queries/$query_id/cancel
{%- endcodetabs %}


This endpoint cancels a query started by the [run query endpoint](#running-a-query). The `query_id` must correspond to the id returned by the run query endpoint.


#### HTTP Request

`POST /api/queries/query_id/cancel`

#### Response

```json
{
  "success": true
}
```

The API return value is the success information.
