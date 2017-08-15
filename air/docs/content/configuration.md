# Overview

Before starting the system, you need to configure the Insights Air and Insights Cloak components. Both components are configured through a file called `config.json`. Each component requires its own `config.json` file which must be placed in a separate folder. In other words, you can't have a single `config.json` for both components. When starting each component, you need to mount the folder containing this file, as explained in the [Installation guide](installation.md).

## Insights Air configuration

The Insights Air configuration needs to provide the following information:

- database connection parameters (required)
- web site configuration (required)
- Insights Air PostgreSQL interface parameters (optional)

The general shape of `config.json` is therefore:

```
{
  "database": {
    ...
  },
  "site": {
    ...
  },
  "psql_server": {
    ...
  },
}
```

### Database configuration

The database configuration is used to specify connection parameters for the database used by the Insights Air component to store data such as users, groups, history of queries, and other. The database has to be hosted on a PostgreSQL server version 9.4 or higher.

This section looks as follows:

```
"database": {
  "host": string,
  "port": integer,
  "ssl": boolean,
  "user": string,
  "password": string,
  "name": string
}
```

### Web site configuration

This part of the configuration is used to configure the web server of the Insights Air component. The shape of this section is as follows:

```
"site": {
  "auth_secret": secret_string,
  "endpoint_key_base": secret_string,
  "api_token_salt": secret_string,
  "customer_token": string,
  "master_password": string,
  "certfile": string,
  "keyfile": string
},
```

In the snippet above, the type `secret_string` indicates a string which should consist of at least 64 characters. The corresponding parameters are used to sign and encrypt various data sent to the client. Make sure to choose values which are random enough, or otherwise the security of the system might be compromised. For example, to generate a random secret, you can use the following command:

```
cat /dev/urandom |
  LC_CTYPE=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```

The customer token is the token provided by Aircloak. This value uniquely identifies your Insights Air installation with the central Aircloak systems.

The `master_password` parameter specifies the password (in clear text) which is required when creating the first administrator user. When you attempt to access the site for the very first time, there are no users in the database. At this point, the system will ask you to create the first administrator user, and it will require you to enter the `master_password`. Once the first administrator is created, this password will not be needed anymore.

The final two parameters `certfile` and `keyfile` are optional. They are used to specify the certificate and key for the HTTPS interface. If these parameters are provided, you will also need to put the corresponding files in the same folder as the `config.json` file. Once you do that, the site will accept HTTPS traffic as well as HTTP traffic. If you omit these parameters, the site will only accept HTTP traffic.

The ports on which the site will listen are hardcoded. HTTP traffic is served via port 8080, while HTTPS is served via 8443. As explained in the [Installation guide](installation.md#insights-air), you can use Docker port mapping option to decide which of these two ports you want to expose, and to choose different port numbers on the host server.

We strongly suggest to use only HTTPS for communication between the clients (browsers) and the server (the Insights Air component). Otherwise, the security of the system might be compromised.

### Insights Air PostgreSQL interface configuration

This part of the configuration allows you to instruct the Insights Air component to accept requests over the PostgreSQL wire protocol. If this is configured, Insights Air can be queried from client applications which understand this protocol, such as Tableau.

The configuration consists of the following parameters:

```
"psql_server": {
  "require_ssl": boolean,
  "certfile": string,
  "keyfile": string
}
```

The `require_ssl` parameter specifies whether the connection requires all clients to connect over SSL. If this value is `true`, you also need to provide `certfile` and `keyfile` parameters which specify the file names of the certificate and the key. These files need to be placed in the same folder as the `config.json` file.

If `require_ssl` is false, then the server will accept TCP connection as well as SSL. However, if `certfile` and `keyfile` parameters are not provided, then the server will only work with unencrypted TCP connections.

Regardless of which transport protocol(s) are allowed, the server will always accept requests on the port 8432. As explained in the [Installation guide](installation.md#insights-air), you can use the docker port mapping to expose this port to the outside world.

Once the component is started, you can test the connectivity with the `psql` command line tool:

```
psql -h insights_air_ip_address -p postgresql_interface_port -d data_source_name -U user_name

Where `postgresql_interface_port` is the PostgreSQL interface port provided when the component is started, as explained in the [Installation Guide](installation.md#insights-air).
```

In order for the above command to work, the cloak component must be started as well, and the user must have permissions to query the given data source.

## Insights Cloak configuration

The Insights Cloak configuration is used to provide the following information:

- URL where the Insights Air component can be reached
- Anonymization salt
- Data sources which can be queried

The general shape of `config.json` is:

```
{
  "air_site": string,
  "salt": string,
  "data_sources": [
    configuration_for_data_source_1,
    configuration_for_data_source_2,
    ...
  ]
}
```

The `air_site` parameter holds the URL where Insights Air component can be reached. It can be in the form of `"ws://air_host_name:port"` or `"wss://air_host_name:port"`, where `air_host_name` is the address of the machine where the Insights Air component is running. You should use the `ws` prefix if Insights Air is serving traffic over HTTP, while `wss` should be used for the HTTPS protocol.

The `salt` parameter is used for anonymization purposes. Make sure to create a strongly random secret for this parameter, for example with the following command:

```
cat /dev/urandom |
  LC_CTYPE=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```

### Data source configuration

The `data_sources` section configures the databases and tables that will be made available to analysts for querying. Each value in the `data_sources` list is a json in the form of:

```
{
  "name": string,
  "driver": string,
  "parameters": {
    "hostname": string,
    "port": integer,
    "username": string,
    "database": string,
    "password": string
  },
  "tables": tables
}
```

The `name` parameter is a string which will be used to identify the data source throughout the Insights Air interface and APIs.

The `driver` parameter can be one of the following: `mongodb`, `postgresql`, `mysql`, `sqlserver`, `saphana`. The `parameters` json, then specifies the database connection parameters.

Some of these drivers use ODBC protocol to talk to the database. These drivers are `sqlserver` and `saphana`. Since they rely on ODBC, they accept some additional optional connection parameters:

  - `encoding` which has possible values of "latin1", "unicode", "utf8", "utf16", "utf32", "utf16-big", "utf16-little", "utf32-big", "utf32-little".
  - `odbc_parameters` - ODBC specific parameters for the ODBC driver which is used to talk to the database.

The database tables that should be made available for querying are defined in the `tables` section of the cloak config. The value of the `tables` key is a JSON object that looks as follows:

```
"tables": {
  table_name_1: {
    "db_name": string,
    "user_id": string,
    "ignore_unsupported_types": boolean
  },
  table_name_2: ...
}
```

Each `table_name_x` key specifies the name the table will be available under when querying the data source through Aircloak. The `db_name` is the name of the table in the underlying database. In most cases you can use the same name, but the distinction allows some special scenarios, such as exposing a table under a simpler name, or exposing the same database table multiple times under different names.

The `user_id` field is the name of the column that uniquely identifies users - the people or entities whose anonymity should be preserved.

Finally, `ignore_unsupported_types` specifies how to handle columns with unsupported types. If the value is `true`, the cloak will ignore such columns. If this value is `false`, the cloak will refuse to start if there are one or more columns of an unsupported data type.

#### Projected tables

In some cases a table does not have a `user_id` column but is related to another table that does. You could for example have an `accounts` table with a `user_id` column, and a table named `transactions` with an `account_id` column.

To get a `user_id` column in the `transactions` table, the cloak needs to be configured to derive it from the `accounts` table:

```
"tables": {
  "accounts": {
    "db_name": "accounts",
    "user_id": "customer_id"
  },
  "transactions": {
    "db_name": "transactions",
    "projection": {
      "table": "accounts",
      "foreign_key": "account_id",
      "primary_key": "id"
    }
  },
  ...
}
```

Here, we are specifying that the `transactions` table derives its `user_id` column from the `accounts` table. The `account_id` field of the `transactions` table corresponds to the `id` field of the `accounts` table. This results in the `transactions` table getting an extra column called `customer_id`.

#### Table sample rate (only for MongoDb)

For MongoDb databases, every collection is initially scanned to determine the collection schema. This can take a long time for larger collections, which might lead to increased cloak startup times. You can instruct the cloak to analyze only a fraction of the data in the MongoDb collection by providing the `sample_rate` option:

```
"tables": {
  "some_table": {
    "sample_rate": sample_rate,
    ...
  },
  ...
}
```

Where `sample_rate` is an integer between 1 and 100, representing the percentage of data which is going to be sampled.

#### Data decoding

Some tables might contain data encoded in a way that it is difficult or impossible to work with directly by analysts. The cloak has support for decoding the data before a query is executed. The decoders are set in the table section. Each one will be applied, sequentially, in the specified order. If an error is encountered by a decoder, processing is skipped for that decoder and the input value is returned (for example, if a decoder tries to decrypt a value and it fails, the original value is kept during further processing; this is useful in case the column holds mixed (encrypted and plain-text) values).

Be careful when using this feature and only decode data in the cloak when there is no other alternative, as any query accessing an encoded column will have greatly reduced performance, due to the cloak not being able to offload processing to the database server.

To configure a decoder, you need to provide the `decoders` section under the table configuration:

```
tables": {
  "some_table": {
    "decoders": [
      decoder_1,
      decoder_2,
      ...
    ],
    ...
  },
  ...
}
```

Each decoder is a json in the shape of:

```
{"method": string, "columns": ["some_column_name", ...], additional_parameters}
```

The `method` parameter can have one of the following values: `aes_cbc_128`, `base64`, `text_to_integer`, `text_to_real`, `text_to_datetime`, `text_to_date`, `text_to_time`.

The `columns` parameter holds a list of columns which must be decoded with the given method.

Finally, depending on the decoding method, you might need to provide additional parameters.

The `aes_cbc_128` decoder requires the `key` parameter which holds the decryption key:

```
{"method": "aes_cbc_128", "columns": ["some_column_name", ...], "key": "some_decryption_key"}
```

For remaining possible decoders, no additional parameters are needed:

```
{"method": "base64", "columns": ["some_column_name", ...]},
{"method": "text_to_integer", "columns": ["another_column_name", ...]},
...
```
