---
title: Aircloak REST API Reference

language_tabs:
  - ruby

toc_footers:
  - <a href='#'>Sign Up for an API Key</a>

includes:
  - errors

search: true
---

# Introduction

This is the guide for the Aircloak cloak and web REST APIs. You can use these APIs to perform actions such as executing tasks against a cloak cluster, or access results of previous task executions.

All API access is authenticated. Please have a look at the authentication section for details.

The examples in this guide are given in ruby. If it is not clear how this applies to your own environment,
please reach out to us on [solutions@aircloak.com](mailto:solutions@aircloak.com), and we will be happy to assist.

# Task results

## Get results for a specific task

```ruby
require 'net/http'
require 'uri'
require 'openssl'
require 'json'

def http_get(path, api_key)
  uri = URI.parse(path)
  http = Net::HTTP.new(uri.host, uri.port)
  http.read_timeout = 300
  http.use_ssl = true
  http.key = api_key.key
  http.cert = api_key.certificate
  http.verify_mode = OpenSSL::SSL::VERIFY_NONE
  http.get(uri.request_uri)
end

key_file_name = "my_api_key.pfx"
key_password = "my_password"
api_key = OpenSSL::PKCS12.new(File.read(key_file_name), key_password)
response = http_get("https://hello.aircloak.com/api/task_results/1", api_key)
```

> The above command returns JSON structured like this:

```json
{
  "success":true,
  "count":30,
  "page":1,
  "per_page":10,
  "items":[
    {
      "published_at":1421660484000,
      "buckets":[
        {"name":"height: 140","value":20},
        {"name":"height: 160","value":35}
      ],
      "exceptions":[]
    },
    {
      "published_at":1421660484000,
      "buckets":[
        {"name":"height: 180","value":25},
        {"name":"height: 200","value":25}
      ],
      "exceptions":[]
    }
  ]
}
```

This endpoint retrieves results for the given task. Retrieved results are ordered (newest come first) and paginated.

### HTTP Request

`GET /api/task_results/task_id`

<aside class="notice">
In the path, you must replace `task_id` with the real id of the task.
</aside>

### Query Parameters

Parameter | Default | Description
--------- | ------- | -----------
page      | 1       | Page number
per_page  | 10      | The number of items per page
