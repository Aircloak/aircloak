---
title: Aircloak REST API Reference

language_tabs:
  - ruby

includes:
  - errors

search: true
---

# Introduction

This is the guide for the Aircloak cloak and web REST APIs. You can use these APIs to perform actions such as executing tasks against a cloak cluster, or access results of previous task executions.

All API access is authenticated. Please have a look at the authentication section for details.

The examples in this guide are given in ruby. If it is not clear how this applies to your own environment,
please reach out to us on [support@aircloak.com](mailto:support@aircloak.com), and we will be happy to assist.


# Authentication

All APIs, whether on the cloak or in the Aircloak web interface, are authenticated using client certificates.

> An example of how perform a HTTP GET request in ruby using client certificates

```ruby
require 'net/http'
require 'uri'
require 'openssl'
require 'json'

class RestClient
  def self.key_from_file path, password
    OpenSSL::PKCS12.new File.read(path), password
  end

  def self.get path, api_key
    uri = URI.parse path
    http = Net::HTTP.new uri.host, uri.port
    http.read_timeout = 300
    http.use_ssl = true
    http.key = api_key.key
    http.cert = api_key.certificate
    http.verify_mode = OpenSSL::SSL::VERIFY_NONE
    response = http.get uri.request_uri
    JSON.parse response.body
  end

  def self.post path, payload, api_key, headers = {}
    uri = URI path
    https = Net::HTTP.new uri.host, uri.port
    https.use_ssl = true
    https.key = api_key.key
    https.cert = api_key.certificate
    https.verify_mode = OpenSSL::SSL::VERIFY_NONE
    request = Net::HTTP::Post.new uri.path
    headers.each_pair do |header_name, value|
      request.add_field header_name.to_s, value
    end
    response = https.request request, payload
    JSON.parse response.body
  end
end
```

To manage your keys, please visit the [keys](/keys) section in our web interface.

There are four types of key supported by our system. They differ in the scope of permissions they allow. The
keys are:

- data upload keys
- admin key
- key for running tasks
- key for issuing REST API calls against the web

With the exception of the _key for issuing REST API calls against the web_, all the keys above are used
against our cloaks.


### Data upload keys

The data upload key comes in two varieties. One which allows bulk uploading of data for __any user__, and one which only allows data upload for a __particular, specified user__.

The key which can be used for __any user__ is best suited for environments where the key can be strongly protected. Such environments include your own secure server systems.

Should this key get exposed, the adversary could irreversibly corrupt your database by uploading significant
amounts of bogus information. As you never see raw data once it has been uploaded, the extent to which your
database has been corrupted can be hard to quantify, and counteract.

Therefore, if there is a chance your key might have been exposed, please immediately revoke it in the
[keys](/keys) section of our web interface.

The key for uploading data for a __particular, specified user__, only allows data upload for the user that was
specified when the key was generated.
These keys are well suited for deployments where you are uploading data from deployed clients, where you have
less control over the key material.

These keys are generated through an authenticated API. Please contact us on
[support@aircloak.com](mailto:support@aircloak.com) should you be interested.

# Web REST API

## Get list of all tasks

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
url = "https://api.aircloak.com/tasks"
response = RestClient.get(url, api_key)
```

This endpoint retrieves all tasks for the authenticated analyst.

### HTTP Request

`GET /tasks`

### Authentication

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

```json
{
  "success": true,
  "tasks": [
    {"token": "token_1", "type": "batch", "name": "task_1", "cluster_name": "cluster 1"},
    {"token": "token_2", "type": "streaming", "name": "task_2", "cluster_name": "cluster 1"},
    {"token": "token_3", "type": "batch", "name": "task_3", "cluster_name": "cluster 2"}
  ]
}
```

The API return value is a list of all tasks for the authenticated analyst.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.

## Get results for a specific task

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
task_token = "my_task_token"
url = "https://api.aircloak.com/tasks/#{task_token}/results"
response = RestClient.get(url, api_key)
```

This endpoint retrieves results for the given task. Retrieved results are ordered (newest come first) and paginated.

### HTTP Request

`GET /tasks/<task-token>/results`

<aside class="notice">
In the path, you must replace &lt;task-token&gt; with the real token of the task. You can obtain this token when <a href="#get-list-of-all-tasks">retrieving the list of all tasks</a>.
</aside>

### Query Parameters

Parameter | Required | Default | Description
--------- | -------- | ------- | -----------
page      | false    | 1       | Page number
per_page  | false    | 10      | The number of items per page

### Authentication

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

```json
{
  "success":true,
  "count":30,
  "page":1,
  "per_page":10,
  "results":[
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
The API return value is a list of results on the given page. Furthermore, information about the current page, and count of all results are included, in case you want to display a pagination user interface.



# Cloak APIs

All cloak APIs are authenticated. Please see the [authentication section](#authentication) for more information.

The paths given below are all absolute paths. In the web interface you get the fully qualified names of the
cloaks in your cluster. These are of the form `machineName.cloak.aircloak.net`. Given the path `/PATH` in an
API description, you are expected that you make a request to
`https://machineName.cloak.aircloak.net/PATH`.

If you have multiple machines available in your cluster, the particular machine endpoint you use does not
matter.


## Single user data insert

```ruby
json_payload = <<-EOJSON
  {
    "locations": [
      {"x": 1, "y": 1},
      {"x": 2, "y": 2}
    ],
    "purchases": [
      {"product": "washing machine", "price": 10000}
    ]
  }
EOJSON

api_key = RestClient.key_from_file "insert-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.net/insert"
RestClient.post url, json_payload, api_key
```

The single user API endpoint is useful if you want to upload data directly from a single users device.
For example a mobile application that only deals with the data of that particular user.

If you want to upload data for multiple users at the same time, have a look at the [bulk
upload](#bulk-insert)
API.

Also consider having a look at the [data upload](/help/introduction-uploading-data) and the [managing
data](/help/managing-data) help pages for a more in-depth guide to how tables are created, data formatted, and
naming conventions and restrictions.


### HTTP Request

`POST /insert`

### Payload

The API endpoint expects data to be a JSON object.
The data is expected to contain the table name as a key. The value of the key is a list of
rows to be inserted in the database.
Each row is a JSON object with key-value pairs, where the key is the column name, and the value,
a JSON encoded value.

The API accepts gzipped payloads. The `Content-Encoding` header should be set to `gzip` if the data is
compressed.

The examples used assume you have two tables defined with the following table structures:

#### locations

| column name | data type |
|-------------|-------------|
| x | integer |
| y | integer |

#### purchases

| column name | data type |
|-------------|-------------|
| product | varchar(255) |
| price | integer |


### Authentication

You need a single user API key to access this API endpoint.
Please contact us on [support@aircloak.com](mailto:support@aircloak.com) if you are interested in getting
keys for single users and want to learn more about the integration required.

### Response

```json
{"success": true}
```

The API return value is a JSON object with a boolean field called success. If it is set to false, the returned
object also contains an error field describing the reason why the operation failed.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.

### Restrictions

Payloads exceeding 10mb in size will be rejected.


## Bulk insert

```ruby
# Assumes the cloak has table structures like in the
# single user insert example
json_payload = <<-EOJSON
  {
    "user1": {
      "locations": [
        {"x": 1, "y": 1},
        {"x": 2, "y": 2}
      ]
    },
    "user2": {
      "locations": [{"x": 2, "y": 2}],
      "purchases": [
        {"product": "washing machine", "price": 10000}
      ]
    }
  }
EOJSON

api_key = RestClient.key_from_file "bulk-insert-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.net/bulk_insert"
RestClient.post url, json_payload, api_key
```

The bulk insert API is useful when uploading data from a system where you have access to data for
multiple users at the same time. It performs better than the single user insert API, and
highly recommended if you want to upload large amounts of data.

If you want to upload data for a single user from a client device, have a look at the [single user insert API](#singel-user-data-insert)
API.

Also consider having a look at the [data upload](/help/introduction-uploading-data) and the [managing
data](/help/managing-data) help pages for a more in-depth guide to how tables are created, data formatted, and
naming conventions and restrictions.


### HTTP Request

`POST /bulk_insert`

### Payload

When uploading data for multiple users, you upload a JSON object, where each key corresponds to the user id of
the user you are uploading data for. The value for the user is the users data, formatted like it is described
in the section above on how to format data for individual users.

The API accepts gzipped payloads.


### Authentication

A Data upload key with privileges to upload data for _any_ user must be used.
See the [authentication](#authentication) section for details.

### Response

```json
{"success": true}
```

The API return value is a JSON object with a boolean field called success. If it is set to false, the returned
object also contains an error field describing the reason why the operation failed.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.

### Restrictions

Payloads exceeding 10mb in size will be rejected.


## Task execution

```ruby
json_payload = <<-EOJSON
  {
    "prefetch": [
      {"table": "locations"},
      {
        "table": "purchases",
        "user_rows": 10,
        "time_limit": 10,
        "where": [
          {"$$price": {"$gt": 100}}
        ]
      }
    ],
    "post_processing": {
      "code": "report_property('user with', 'expensive purchase')"
    }
  }
EOJSON

headers = {
  async_query: true,
  auth_token: "ABCDEFGH",
  return_url: "aHR0cHM6Ly9lbmQtcG9pbnQuZXhhbXBsZS5jb20v",
  task_id: "my-task"
}

api_key = RestClient.key_from_file "task-running-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.net/task/run"
RestClient.post url, json_payload, api_key, headers
```

This API endpoint allows execution of batch tasks against a cloak cluster.

Tasks can either be run asynchronously or synchronously.
Calls to run tasks asynchronously immediately return. Once available, the results are sent to an HTTP endpoint which was specified along with the task.
Calls to run tasks synchronously block until the results are available, or until they time out.

<aside class="notice">
Asynchronous tasks can only use the Aircloak HTTP endpoint at this time. This is due to how our networks are
secured.
</aside>

The API assumes you are familiar with writing queries against the Aircloak platform. If not, please have
a look at the following help pages for an introduction:

- [Understanding the query model](/help/understanding-the-query-model)
- [An introduction to writing queries](/help/introduction-queries)
- [Query examples](/help/query-examples)


### HTTP Request

`POST /task/run`

#### HTTP headers

Header           | Default | Description
---------------- | ------- | -----------
async_query      | false   | If true, the call returns immediately, and the result are sent to the `return_url`
auth_token       |         | __Required for asynchronous tasks__. The results are sent back with the HTTP header `QueryAuthToken` set to the value of the `auth_token` header
return_url       |         | __Required for asynchronous tasks__. The url that the results are sent to. For the time being non-aircloak return urls will silently fail. __The `return_url` must be a base64 encoded string__.
task_id          |         | __Required for asynchronous tasks__. The `task_id` is returned as part of the result payload


### Payload

The payload should be a JSON object with two keys: `prefetch` and `post_processing`.
The prefetch clause is used to select data from the database. The post processing clause is executed once per
user over the data returned by the prefetch clause.

The post processing clause should contain a `code` section containing the lua task to execute.

#### Prefetch

The prefetch should be a list of objects, where each objects have the following keys

Key        | Required | Default  | Description
---------- | -------  | -------- | -------------
table      | true     |          | The name of the table to fetch data from. This name corresponds to the name used to define the table in the web interface
user_rows  | false    | all rows | Limits the number of rows returned per user. If not specified, all matching rows per user are returned
time_limit | false    | from beginning of time | If specified, restricts the data to rows uploaded within the last `time_limit` seconds. It does not take any semantic timestamp columns that might be present in the data into account
where      | false    | no filter | If specified, returns only database rows for which the filter is true

##### Where clause

> Example where clause: x < 10 and y >= 5

```json
[
  {"$$x": {"$lt": 10}},
  {"$$y": {"$gte": 5}}
]
```

> Example where clause: (name == 'sam' and age > 10) or (age < 50 and (salary > 10 or lucky == true))

```json
[
  {"$or": [
    [
      {"$$name": {"$eq": "sam"}},
      {"$$age": {"$gt": 10}}
    ],
    [
      {"$$age": {"$lt": 50}},
      {"$or": [
        {"$$salary": {"$gt": 10}},
        {"$$lucky": {"$eq": true}}
      ]}
    ]
  ]}
]
```
Naming conventions: when referencing a column name, prefix it with two dollar signs. i.e. the column named `x`
becomes `$$x`. When referencing an operator like `or`, prefix it with a single dollar sign. `or`
becomes `$or`.

The following operators are supported:

Operator | Description
-------- | -----------
or       | True if either of the operands hold true
eq       | True if the operands are equal
neq      | False if the operands are equal
gt       | True if the specified column is greater than the operand
gte      | True if the specified column is greater than or equal to the operand
lt       | True if the specified column is less than the operand
lte      | True if the specified column is less than or equal to the operand

Please have a look at the examples for further details.

##### Order of filters

When you supply a combination of `time_limit`, `user_rows` and `where`-conditions, the `where` and
`time_limit` clauses are applied first. The `user_rows` limit is applied per user in the resulting
dataset.


### Authentication

Requires a key of type __key for running tasks__. Please have a look at the [authentication](#authentication) section
for more information about the different key types, or go to the [keys](/keys) page to manage your keys.

### Response

#### Asynchronous tasks

```json
{"success": true}
```

The response is a JSON object with the key `success` containing a boolean value. If the `success` is false, an
error key will also be present which explains why the task could not be executed.

#### Synchronous tasks

> Result of a successful synchronous task invocation

```json
{
  "analyst_id": 1,
  "task_id": "abcd",
  "buckets": [
    {"label":"Country", "value":"Norway", "count": 1050},
    {"label":"Country", "value":"Germany", "count": 9500}
  ],
  "exceptions": [
    {"error":"exception that occurred", "count": 5}
  ]
}
```

Synchronous tasks if successful, will return a JSON object containing the result under the key
`buckets`. Any exceptions that were raised in sufficient quantity to pass through the anonymization filters are included
under the `exceptions` key. The task id as well as analyst id are also present.

If a synchronous task fails, it will return a JSON object with the `success` key set to false, and an `error`
key describing the problem.

### Restrictions

Synchronous tasks time out after 2 minutes.
If your task needs longer, please consider either running it as an asynchronous task, or adding more machines to
your cluster.
