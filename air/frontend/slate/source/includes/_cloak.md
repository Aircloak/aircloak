# Cloak APIs

All cloak APIs are authenticated. Please see the [authentication section](#authentication) for more information.

The paths given below are all absolute paths. In the web interface you get the fully qualified names of the
cloaks in your cluster. These are of the form `machineName.cloak.aircloak.com`. Given the path `/PATH` in an
API description, you are expected that you make a request to
`https://machineName.cloak.aircloak.com/PATH`.

If you have multiple machines available in your cluster, the particular machine endpoint you use does not
matter.

When sending a payload to a cloak, explicitly state its content-type. The cloaks will reject payloads where
the content-type is not specified, or the type is one not supported.
All APIs support the payload encoded as JSON. The accepted content-type for JSON is `application/json`.


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

# The cloak requires the content type of the
# uploaded data to be explicitly stated.
headers = {
  "Content-Type" => "application/json"
}

api_key = RestClient.key_from_file "insert-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.com/insert"
RestClient.post url, json_payload, api_key, headers
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
     --header='Content-Type: application/json' \
     https://<cloak-server>.cloak.aircloak.com/insert
```

```plaintext
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

curl -k -X POST
    --data-binary @locations.json \
    --cert <path-to-PEM-certificate> \
    -H 'Content-Type: application/json' \
    https://<cloak-server>.cloak.aircloak.com/insert
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

#### HTTP headers

Header           | Required | Default  | Description
---------------- | -------- | -------- | -----------
Content-Type     | true     |          | Both `application/json` and `application/msgpack` are supported
Content-Encoding | false    | identity | Can be set to `gzip` if the payload is gzipped


### Payload

The API endpoint expects data to be a JSON object and expects the header `Content-Type: application/json` to
be set.
The data is expected to contain the table name as a key. The value of the key is a list of
rows to be inserted in the database.
Each row is a JSON object with key-value pairs, where the key is the column name, and the value,
a JSON encoded value.

The API accepts gzipped payloads. The `Content-Encoding` header should be set to `gzip` if the data is
compressed (`Content-Encoding: gzip`).

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
{"success": false, "errors": "unknown content type"}
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

Payloads exceeding 10 MB in size will be rejected.


## Bulk insert

```ruby
# Assumes the cloak has table structures like in the single user insert example.
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

# The cloak requires the content type of the uploaded data to be explicitly stated.
headers = {
  "Content-Type" => "application/json"
}

api_key = RestClient.key_from_file "bulk-insert-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.com/bulk_insert"
RestClient.post url, json_payload, api_key, headers
```

```shell
# Assumes the cloak has table structures like in the single user insert example.
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
     --certificate=<path-to-PEM-certificate> \
     --body-file=locations.json \
     --no-check-certificate \
     --header='Content-Type: application/json' \
     https://<cloak-server>.cloak.aircloak.com/bulk_insert
```

```plaintext
# Assumes the cloak has table structures like in the single user insert example.
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

curl -k -X POST
    --data-binary @locations.json \
    --cert <path-to-PEM-certificate> \
    -H 'Content-Type: application/json' \
    https://<cloak-server>.cloak.aircloak.com/bulk_insert
```

The bulk insert API is useful when uploading data from a system where you have access to data for
multiple users at the same time. It performs better than the single user insert API, and
highly recommended if you want to upload large amounts of data.

If you want to upload data for a single user from a client device, have a look at the [single user insert API](#single-user-data-insert)
API.

Also consider having a look at the [data upload](/help/introduction-uploading-data) and the [managing
data](/help/managing-data) help pages for a more in-depth guide to how tables are created, data formatted, and
naming conventions and restrictions.


### HTTP Request

`POST /bulk_insert`

#### HTTP headers

Header           | Required | Default  | Description
---------------- | -------- | -------- | -----------
Content-Type     | true     |          | Both `application/json` and `application/msgpack` are supported
Content-Encoding | false    | identity | Can be set to `gzip` if the payload is gzipped


### Payload

When uploading data for multiple users, you upload a JSON object, where each key corresponds to the user id of
the user you are uploading data for. The value for the user is the users data, formatted like it is described
in the section above on how to format data for individual users.

The cloak expects the header `Content-Type: application/json` to be set.

The API accepts gzipped payloads as well. If you gzip your payload, you also have to add the
`Content-Encoding: gzip` header.


### Authentication

A Data upload key with privileges to upload data for _any_ user must be used.
See the [authentication](#authentication) section for details.

### Response

```json
{"success": true}
{"success": false, "errors": "unknown content type"}
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

Payloads exceeding 10 MB in size will be rejected.

Data is atomically inserted per-user. So for a single user, you either get all the uploaded data inserted or nothing.
When issuing a bulk insert request for multiple users, there is no way to have everything inserted or nothing,
as the data will end up in different nodes in the cluster.
If you need to know which user's data failed to be uploaded, you need to either split the data into multiple
single-user batches or parse the returned output for errors.


## Stream insert

```ruby
# Assumes the cloak has table structures like in the single user insert example.
csv_payload = <<-EOCSV
user_id,product,price
user1,"Washing Machine",1000
user1,"Smart TV",600
user2,"Microwave oven",200
EOCSV

# The cloak requires the content type of the
# uploaded data to be explicitly stated.
headers = {
  "Content-Type" => "text/csv"
}

api_key = RestClient.key_from_file "bulk-insert-key", "password"
url = "https://<MACHINE-NAME>.cloak.aircloak.com/stream_insert/purchases"
RestClient.post url, json_payload, api_key, headers
```

```shell
# Assumes the cloak has table structures like in the single user insert example.
cat > purchases.csv <<EOCSV
user_id,product,price
user1,"Washing Machine",1000
user1,"Smart TV",600
user2,"Microwave oven",200
EOCSV

wget --content-on-error \
     --output-document - \
     --method=POST \
     --certificate=<path-to-PEM-certificate> \
     --body-file=purchases.csv \
     --no-check-certificate \
     --header='Content-Type: text/csv' \
     https://<cloak-server>.cloak.aircloak.com/stream_insert/purchases
```

```plaintext
# Assumes the cloak has table structures like in the single user insert example.
cat > purchases.csv <<EOCSV
user_id,product,price
user1,"Washing Machine",1000
user1,"Smart TV",600
user2,"Microwave oven",200
EOCSV

curl -k -X POST
    --data-binary purchases.csv \
    --cert <path-to-PEM-certificate> \
    -H 'Content-Type: text/csv' \
    https://<cloak-server>.cloak.aircloak.com/stream_insert/purchases
```

The stream insert API is useful when uploading very large amounts of data into a single table.
Because the data is inserted as it is received, this endpoint will be faster and it will handle
larger requests than the [bulk insert](#bulk-insert) endpoint.

If you want to upload data into multiple tables at the same time, have a look at the
[single user insert API](#single-user-data-insert) and [bulk insert](#bulk-insert) APIs.

Also consider having a look at the [data upload](/help/introduction-uploading-data) and the
[managing data](/help/managing-data) help pages for a more in-depth guide to how tables are created,
data formatted, and naming conventions and restrictions.


### HTTP Request

`POST /stream_insert/<table_name>`

#### HTTP headers

Header           | Required | Default  | Description
---------------- | -------- | -------- | -----------
Content-Type     | true     |          | Only `text/csv` is supported
Content-Encoding | false    | identity | Can be set to `gzip` if the payload is gzipped


### Payload

The cloak expects a payload with the CSV format, where the first row specifies the columns
which are to be inserted. All the mandatory columns in the table have to be referenced.
The first column is considered the user ID and can have any name.
Text values containing commas or quotes have to be quoted. Quotes must be escaped using double-quotes.
For example, the string `Say "hello","bye" or nothing!` would become `"Say ""hello"",""bye"" or nothing!"`

The cloak expects the header `Content-Type: text/csv` to be set.

The API accepts gzipped payloads as well. If you gzip your payload, you also have to add the
`Content-Encoding: gzip` header.


### Authentication

A Data upload key with privileges to upload data for _any_ user must be used.
See the [authentication](#authentication) section for details.

### Response

```json
{"inserted_rows":3}
{"inserted_rows":0,"error":"table does not exist"}
{"inserted_rows":1245,"error":"failed to parse value 'abc' as 'int4' for column 'price'"}
```

The API return value is a JSON object with an integer field called `inserted_rows` that specifies
the count of successfully stored data rows. Row order is preserved, so this value refers to the first
rows following the columns definition. In case of success, the return HTTP status code will be `200 OK`,
otherwise a non-2XX status code is set and the returned JSON object also contains information about the
error that occurred. After fixing the error, the upload request can be re-issued after removing the
already stored rows.

For the use of error codes in the Cloak API, please consult the [Errors](#errors) section.


### Restrictions

Payloads exceeding 50 GB in size will be rejected. An 24 hours processing timeout also applies.

This API can accept very large requests, as the data is inserted as it arrives.
Regardless, to improve reliability, we recommend splitting the upload into chunks smaller than 1 GB.