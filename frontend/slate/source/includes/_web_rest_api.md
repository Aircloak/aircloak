# Web REST API

## Get list of all clusters

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
url = "https://api.aircloak.com/clusters"
response = RestClient.get(url, api_key)
```

```shell
wget --content-on-error \
     --output-document - \
     --certificate=<path-to-PEM-certificate> \
     https://api.aircloak.com/clusters

  OR

curl --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/clusters
```

This endpoint retrieves all available clusters for the authenticated analyst.

### HTTP Request

`GET /clusters`

### Authentication

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

```json
{
  "success": true,
  "clusters": [
    {"id":1, "name": "Artemis"},
    {"id":2, "name": "Zeus"}
  ]
}
```

The API return value is a list of all clusters for the authenticated analyst.

For the use of error codes in the Web REST API, please consult the [Errors](#errors) section.


## Get list of all tasks

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
url = "https://api.aircloak.com/tasks"
response = RestClient.get(url, api_key)
```

```shell
wget --content-on-error \
     --output-document - \
     --certificate=<path-to-PEM-certificate> \
     https://api.aircloak.com/tasks

  OR

curl --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/tasks
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

For the use of error codes in the Web REST API, please consult the [Errors](#errors) section.

## Task execution overview

There are two distinct ways in which you can execute tasks through the Web REST API.
They have different strengths and trade-offs. It is useful to understand the
differences between the two before making a decision regarding which to use.

The first API allows you to initiate the execution of [existing tasks](#execute-an-existing-task),
tasks that have been defined ahead of time in the [web-interface](/tasks).
The benefit of using tasks that are defined ahead of time is that you have available
a wealth of development aids. These include task testing in a sandbox, auto-completion,
and tools for generating valid data prefetch statements.
Additionally the results of task executions can be inspected in the web-interface itself.

The [second API](#execute-a-dynamic-one-off-task) allows you to pass the task definition along with the request itself.
This is much the same way one would go about directly querying a cloak.
This low-level approach to querying the system has the
benefits that you can dynamically create the task definition to suit your needs.
Results from dynamic one-off tasks are not stored or visible in the web-interface,
and are not available through the [task result API](#get-results-for-a-specific-task).

## Execute an existing task

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
url = "https://api.aircloak.com/tasks/<task-token>/run"
response = RestClient.post(url, nil, api_key)
```

```shell
wget --content-on-error \
     --output-document - \
     --method=POST \
     --certificate=<path-to-PEM-certificate> \
     https://api.aircloak.com/tasks/<task-token>/run

  OR

curl -X POST
    --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/tasks/<task-token>/run
     
```

This endpoint allows you to asynchronously run a _batch task_ that has been defined in the web interface.
If you try to schedule a periodic or streaming task, it will fail. Periodic and streaming tasks are automatically
scheduled on the cloaks upon creation.

The alternatives to this API are to directly run one-off tasks using the [dynamic one-off task API](#execute-a-dynamic-one-off-task) or the equivalent API on the [cloaks themselves](#task-execution). The benefit this API
has over the former two is that you are able to develop and test your task using the online sandbox
functionality and that you have full access to the Aircloak provided standard library of helper functions.

When using this API, the tasks are executed asynchronously on the cloaks. They are not subject
to the strict execution time limits that synchronous tasks executed through the [cloak task execution
API](#task-execution) are subject to. Results can also be browsed in the web interface, and later downloaded
with the [task result API](#get-results-for-a-specific-task).


### HTTP Request

`POST /tasks/<task-token>/run`

### Authentication

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

```json
{
  "success": true
}
```

The API returns a confirmation that the task was scheduled. __Please note__ that this does not mean that the
task has already finished executing.

If the task cannot be scheduled, the response contains a description of the problem.

For the use of error codes used in the Web REST API, please consult the [Errors](#errors) section.


## Execute a dynamic one-off task

```ruby
json_payload = <<-EOJSON
  {
    "cluster": <cluster-id>,
    "prefetch": [
      {"table": "locations"},
      {
        "table": "measurements",
        "user_rows": 10,
        "time_limit": 10,
        "where": [
          {"$$height": {"$gt": 100}}
        ]
      }
    ],
    "post_processing": {
      "code": "height = Aircloak.Utils.quantize(tables.measurements[1].height, 30)\nreport_property('height', height)"
    }
  }
EOJSON

api_key = RestClient.key_from_file "task-running-key", "password"
url = "https://api.aircloak.com/task/run"
RestClient.post url, json_payload, api_key
```

```shell
cat > task.json <<EOJSON
  {
    "cluster": <cluster-id>,
    "prefetch": [
      {"table": "locations"},
      {
        "table": "measurements",
        "user_rows": 10,
        "time_limit": 10,
        "where": [
          {"$$height": {"$gt": 100}}
        ]
      }
    ],
    "post_processing": {
      "code": "height = Aircloak.Utils.quantize(tables.measurements[1].height, 30)\nreport_property('height', height)"
    }
  }
EOJSON

wget --content-on-error \
     --output-document - \
     --method=POST \
     --quiet \
     --certificate=<path-to-PEM-certificate> \
     --body-file=task.json \
     https://api.aircloak.com/task/run

  OR

curl -X POST
    --data-binary @task.json \
    --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/task/run
```

This API endpoint allows execution of batch tasks against a cloak cluster. The primary use case of this API is
when you programmatically want to run dynamically generated queries against the cloak.

Aircloak provided standard library functions can be used in the tasks executed against this API.

The API is synchronous and blocks while waiting for the cloak to perform the cloaked computations.

<aside class="warning">
<strong>Please note</strong>:
With the current incarnation of the API, very long running tasks will time out.
If your tasks require more than 30 seconds to complete, please define them in the web-interface.
You can then invoke them using the API for <a href="#execute-an-existing-task">executing predefined tasks</a>.
</aside>

The API assumes you are familiar with writing queries against the Aircloak platform. If not, please have
a look at the following help pages for an introduction:

- [Understanding the query model](/help/understanding-the-query-model)
- [An introduction to writing queries](/help/introduction-queries)
- [Query examples](/help/query-examples)


### HTTP Request

`POST /task/run`


### Payload

The payload should be a JSON object with three keys: `cluster`, `prefetch` and `post_processing`.
The cluster clause specifies on which of your clusters the task should be executed. The IDs of your
clusters can be gotten through the [/clusters](#get-list-of-all-clusters) API endpoint.
The prefetch clause is used to select data from the database. The post processing clause is executed once per
user over the data returned by the prefetch clause.

The post processing clause must contain a `code` section containing the Lua task to execute.

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

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

> Result of a successful synchronous task invocation

```json
{
  "success": true,
  "result": {
    "published_at":1421660484000,
    "buckets":[
      {"label": "height", "value": "140", "count": 20},
      {"label": "height", "value": "160", "count": 35}
    ],
    "exceptions": [
      {"error":"exception that occurred", "count": 5}
    ]
  }
}
```

The task, if successful, will return a JSON object containing the result under the key
`result`. The result section contains all buckets that passed through anonymisation.
If the task threw exceptions in sufficient quantities, these can be found under exceptions.

If a synchronous task fails, it will return a JSON object with the `success` key set to false,
and a `description` key describing the problem.

### Restrictions

Synchronous tasks time out after 30 seconds.
If your task needs longer, please consider specifying them in the web interface and using
the [API for executing existing tasks](#execute-an-existing-task).

## Get results for a specific task

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
task_token = "my_task_token"
url = "https://api.aircloak.com/tasks/#{task_token}/results"
response = RestClient.get(url, api_key)
```

```shell
wget --content-on-error \
     --output-document - \
     --certificate=<path-to-PEM-certificate> \
     https://api.aircloak.com/tasks/<task-token>/results

  OR

curl --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/tasks/<task-token>/results
```

This endpoint retrieves results for the given task. Retrieved results are ordered (newest come first) and paginated.

### HTTP Request

`GET /tasks/<task-token>/results`

<aside class="notice">
In the path, you must replace &lt;task-token&gt; with the real token of the task. You can obtain this token when <a href="#get-list-of-all-tasks">retrieving the list of all tasks</a>.
</aside>

### Query Parameters

```ruby
# Using RestClient from example in the Authentication section

api_key = RestClient.key_from_file "my_api_key.pfx", "my_password"
task_token = "my_task_token"
url = "https://api.aircloak.com/tasks/#{task_token}/results?page=1&per_page=1"
response = RestClient.get(url, api_key)
```

```shell
wget --content-on-error \
     --output-document - \
     --certificate=<path-to-PEM-certificate> \
     https://api.aircloak.com/tasks/<task-token>/results?page=1&per_page=1

  OR

curl --cert <path-to-PEM-certificate> \
    https://api.aircloak.com/tasks/<task-token>/results?page=1&per_page=1
```

Parameter | Required | Default | Description
--------- | -------- | ------- | -----------
page      | false    | 1       | Page number
per_page  | false    | 10      | The number of items per page
from      | false    | none    | Lowest report time to include in results. Should be provided as YYYYMMDD HH:MM
to        | false    | none    | Highest report time to include in results. Should be provided as YYYYMMDD HH:MM

Parameters are set in the URL as in any HTTP GET request.

For example, because the results are returned in descending order, to get only the latest result for a task
you need to set `page=1` and `per_page=1` in the query string.

### Authentication

You need a REST API key to access this API endpoint. See the [authentication](#authentication) section for details.

### Response

```json
{
  "success":true,
  "count":30,
  "page":1,
  "per_page":10,
  "from": "20150119 09:00",
  "to": "20150119 11:00",
  "results":[
    {
      "published_at":1421660484000,
      "buckets":[
        {"label": "height", "value": "140", "count": 20},
        {"label": "height", "value": "160", "count": 35}
      ],
      "exceptions":[]
    },
    {
      "published_at":1421660484000,
      "buckets":[
        {"label": "height", "value": "180", "count": 25},
        {"label": "height", "value": "200", "count": 25}
      ],
      "exceptions":[]
    }
  ]
}
```
The API return value is a list of results on the given page. Furthermore, information about the current page, and count of all results are included, in case you want to display a pagination user interface.

The format of the results corresponds to the format in which they were reported from the query. If you have
`report_property("gender", "male")` in your query, you will see a row `{"label": "gender",
"value": "male", "count": 5000}` in your results, where the count corresponds to how many distinct users
reported the particular property.


