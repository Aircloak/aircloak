## Data sources

### Get the list of all data sources

```ruby
RestClient.get "#{site_url}/api/data_sources", api_token
```

```shell
wget \
  --content-on-error \
  --output-document - \
  --method=GET \
  --header 'auth-token:'$API_TOKEN \
  $SITE_URL/api/data_sources
```

```curl
curl -v \
  -X GET \
  -H 'auth-token:'$API_TOKEN \
  $SITE_URL/api/data_sources
```

This endpoint retrieves the list of all data sources.

#### HTTP Request

`GET /api/data_sources`

#### Response

```json
[
  {
    "id": "<data-source-id>",
    "name": "<data source name>",
    "tables": [
      {
        "id":"purchases",
        "columns":[
          {"name":"row_id", "type":"integer"},
          {"name":"itemname", "type":"text"},
          {"name":"price", "type":"integer"},
          {"name":"date", "type":"timestamp"}
        ]
      },
      ...
    ]
  },
  ...
]
```

The API return value is a list of all data sources. Take special note of the `id` field. This token must be used when you want to [run a query](#running-a-query) against a data source.

For the use of error codes in the Web REST API, please consult the [Errors](#errors) section.
