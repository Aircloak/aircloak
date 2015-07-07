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

```shell
cat > locations.json <<EOJSON
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

wget --content-on-error \
     --output-document - \
     --method=POST \
     --certificate=<path-to-PEM-certificate> \
     --body-file=locations.json \
     --no-check-certificate \
     https://<cloak-server>.cloak.aircloak.net/insert
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
{"success": false, "errors": "malformed json"}
{"success": false, "errors": {"user-id": ["locations: id: invalid value [1] for type int4"]}}
```

The API return value is a JSON object with a boolean field called success. If it is set to false, the returned
object also contains an error field describing the reason why the operation failed.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.

### Options

Validation mode: you can change the validation mode of the uploaded data, from `strict` to `permissive`,
by setting the `validation_mode` header in the HTTP request. In `strict` mode, which is the default,
the data is inserted only if all the rows passed the validation procedure. In `permissive` mode all valid
rows will be inserted, regardless of if validation errors are present in other parts of the data or not.
In case of a partial data upload, a 202 HTTP status code will be returned.

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

```shell
cat > locations.json <<EOJSON
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

wget --content-on-error \
     --output-document - \
     --method=POST \
     --quiet \
     --certificate=<path-to-PEM-certificate> \
     --body-file=locations.json \
     --no-check-certificate \
     https://<cloak-server>.cloak.aircloak.net/bulk_insert
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
{"success": false, "errors": "malformed json"}
{"success": false, "errors": {"user-id": ["locations: id: invalid value [1] for type int4"]}}
```

The API return value is a JSON object with a boolean field called success. If it is set to false, the returned
object also contains an error field describing the reason why the operation failed.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.

### Options

Validation mode: you can change the validation mode of the uploaded data, from `strict` to `permissive`,
by setting the `validation_mode` header in the HTTP request. In `strict` mode, which is the default,
the data is inserted only if all the rows passed the validation procedure. In `permissive` mode all valid
rows will be inserted, regardless of if validation errors are present in other parts of the data or not.
In case of a partial data upload, a 202 HTTP status code will be returned.

### Restrictions

Payloads exceeding 10mb in size will be rejected.

Data is atomically inserted per-user. So for a single user, you either get all the uploaded data inserted or nothing.
When issuing a bulk insert request for multiple users, there is no way to have everything inserted or nothing,
as the data will end up in different nodes in the cluster.
If you need to know which user's data failed to be uploaded, you need to either split the data into multiple
single-user batches or parse the returned output for errors.


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

# The following headers are required when
# executing a task asynchronously.
# You can also execute the task synchronously
# by leaving the headers blank.
# PLEASE NOTE: asynchronous queries only work
# when run against a cloak you host within your
# own firewall perimeter where the cloak can
# reach the endpoint. When run agaist an
# Aircloak hosted cloak, manually issued
# asynchronous tasks will not work!
headers = {
  async_query: true,
  auth_token: "<Auth token understood by your endpoint",
  return_url: "<Base64-encoded endpoint URL>",
  task_id: "my-task"
}

api_key = RestClient.key_from_file "task-running-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.net/task/run"
RestClient.post url, json_payload, api_key, headers
```

```shell
cat > task.json <<EOJSON
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

# The below headers are required when
# executing a task asynchronously.
# You can also execute the task synchronously
# by leaving out the headers.
# PLEASE NOTE: asynchronous queries only work
# when run against a cloak you host within your
# own firewall perimeter where the cloak can
# reach the endpoint. When run agaist an
# Aircloak hosted cloak, manually issued
# asynchronous tasks will not work!
wget --content-on-error \
     --output-document - \
     --method=POST \
     --quiet \
     --certificate=<path-to-PEM-certificate> \
     --body-file=task.json \
     --header='async_query: true' \
     --header='auth_token: <Auth-token understood by your endpoint>' \
     --header='return_url: <Base64-encoded endpoint URL>' \
     --header='task_id: my-task' \
     --no-check-certificate \
     https://<cloak-server>.cloak.aircloak.net/task/run
```

This API endpoint allows execution of batch tasks against a cloak cluster. Please note that there is also [an
API in the Web REST API](#execute-a-task) which allows you to programatically run tasks defined in the
web interface.

Tasks can either be run asynchronously or synchronously.
Calls to run tasks asynchronously immediately return. Once available, the results are sent to an HTTP endpoint which was specified along with the task.
Calls to run tasks synchronously block until the results are available, or until they time out.

If you have problems with synchronous tasks timing out, and are using a cloak hosted by Aircloak,
consider using the [task execution API in the Web REST API](#execute-a-task) instead. Tasks executed through
the Web REST API are run asynchronously, and are therefore not subject to the strict
timelimits imposed on synchronous tasks executed directly on the cloaks.

<aside class="warning">
<strong>Please note</strong>:
Asynchronous tasks only work when executed on a cloak you have within your own organization's firewall
perimeter! When using a cloak hosted by Aircloak, the firewall configuration prevents the results from
reaching any endpoint you might configure as part of the task metadata, other than the Aircloak Web HTTP API.
This Web HTTP API issues its own auth-tokens, and will not accept ones you generate yourself.
If you need to run asynchronous queries against a cloak hosted by Aircloak, please either consider using the
web interface on <a href="https://hello.aircloak.com">hello.aircloak.com</a>,
or alternatively reach out to Aircloak on <a href="mailto:support@aircloak.com">support@aircloak.com</a>
to establish a workaround.
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
The Lua code must be completely self-contained, and cannot rely on external function definitions
not part of the standard Lua language. Furthermore Aircloak standard libraries as accessible when
defining tasks in the web interface are not known to the cloak, and their usage will result in errors.

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

#### Time limit

Synchronous tasks time out after 2 minutes.
If your task needs longer, please consider either running it as an asynchronous task, or adding more machines to
your cluster.

#### Use of standard libraries and predefined functions

The Aircloak Lua standard libraries as available in the web interface are not known to the cloak.
Using functions like `Aircloak.Utils.quantize(...)` will therefore fail as the cloak
does not have the corresponding function definitions.
If you want to use external libraries in your code you will need to include the full
implementation as part of your task code. Alternatively you can use the [Web REST API equivalent of the task
running api](#execute-a-dynamic-one-off-task) which does automatically include the required standard
library implementations for you.
