# Installation guide

This guide describes how to install and configure the Aircloak components.

## Overview

The Aircloak system consists of two components:

- air - the web site which allows analysts to run anonymized queries in a cloak.
- cloak - the anonymizing query engine.

These components should run on separate machines. In addition, the access to the cloak component should be highly restricted, since this component has complete read access to the sensitive data. The air component does not require such privileges, so you can optionally run it in another network, as long as the cloak component can connect to the air component.

Before installing components, make sure that the following prerequisites are met:

- Docker 1.11 or higher is installed on the host machines.
- The user which installs the components is logged into `quay.io` with `docker login` using credentials provided by Aircloak.
- You have your Aircloak provided `customer-token` available.
- A database in a Postgres server running version 9.4 or higher.
- The air component requires at least 2GB of RAM.
- The cloak component requires at least 8GB of RAM. However, for more complex queries on a larger dataset, more memory might be needed.

## Installing the air component

Before installing the air component, you need the following on your PostgreSQL server:

- a database user for the air component
- a database to which the user has full privileges

You can use arbitrary names for the user and the database.
With the database in place, we can create the air configuration, which looks as follows:

```
{
  "site": {
    "auth_secret": auth_secret,
    "endpoint_key_base": endpoint_key_base,
    "api_token_salt": api_token_salt,
    "master_password": master_password,
    "customer_token": customer_token
  },

  "database": {
    "host": database_host_name,
    "port": database_port_name,
    "ssl": true_or_false,
    "name": database_name,
    "user": user_name,
    "password": user_password
  }
}
```

The configuration needs to be saved under the name `config.json` in some folder.

The configuration consists of the following settings:

- site secrets (`auth_secret`, `endpoint_key_base`, `api_token_salt` under the `site` key) - These are used to sign and encrypt various data exchanged with users of the system. All secrets should consist of at least 64 characters. For example, you can generate a random secret with the following command:
```
cat /dev/urandom |
  LC_CTYPE=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```
- master site password - you will need this password to create the first user in your site
- customer token - this identifies your `air` installation with the central Aircloak systems
- database settings for the air database (host, port, ssl, database name, user name and password)


### Starting the container

Before starting the container, make sure to update the image to the latest version:

```bash
docker pull quay.io/aircloak/air:latest
```

If you want to explicitly control which version you're fetching, then provide the explicit version number instead of the `latest` tag:

```bash
docker pull quay.io/aircloak/air:17.3.0
```

Once air is properly configured you can start the air container with the following command:

```bash
docker run -d --name air \
 -v configuration_folder:/runtime_config \
 -p desired_http_port:8080 \
 --restart=unless-stopped \
 quay.io/aircloak/air:latest
```

In the command above, the `configuration_folder` is the absolute path to the folder where `config.json` is residing.

The `desired_http_port` parameter is the port you want to expose for HTTP requests. It is also possible to expose air over HTTPS. In this case, you need to store `ssl_key.pem` and `ssl_cert.pem` files in the `configuration_folder`. Then, you also need to provide the `-p desired_https_port:8443` option in your docker command, as well as, or instead of, the option for HTTP.

The `--restart=unless-stopped` option specifies a restart policy which ensures that the container is restarted on crash. See [here](https://docs.docker.com/engine/reference/run/#restart-policies-restart) for a more detailed explanation.

The air component also exposes a monitoring endpoint over HTTP. If you want to use it, you need to provide the `-p desired_monitoring_port:8081` option.

If everything was properly configured, you should be able to access air on that port, and create the administrator user using the master password provided in the `config.json`. In the case of problems, you can check the logs with `docker logs air`.

If you want to have explicit control of the air version, replace the `latest` tag in the command with the specific version number.

## Installing the cloak component

Once the air component is setup, we need to create the configuration for the cloak component:

```
{
  "air_site": air_site,
  "salt": salt,
  "data_sources": [
    data_source_1,
    data_source_2,
    ...
  ]
}
```

The configuration needs to be saved under the name `config.json` in some folder.

The `air_site` parameter holds the address of the air site it can be in the form of `"ws://air_host_name:port"` or `wss://air_host_name:port`, where `air_host_name` is the address of the machine where air container is running. You should use the `ws` prefix if air is serving traffic over HTTP, while `wss` should be used for the HTTPS protocol.

The `salt` parameter is used for anonymization purposes. Make sure to create a strongly random secret for this parameter, for example with the following command: `cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1`.

The `data_sources` section configures the databases and tables that will be made available to analysts for querying. Each `data_source_x` is itself a json in the form of:

```
{
  "name": name,
  "driver": driver,
  "parameters": {
    "hostname": database_host,
    "username": database_user,
    "database": database_name,
    "password": password
  },
  "tables": tables
}
```
The `name` parameter is a string which will be used to identiy the data source throughout the Air interface and APIs. The `driver` parameter can be one of the following: `mongodb`, `postgresql`, `mysql`, `sqlserver`, `saphana`, `odbc`. Next, you need to specify the database connection parameters.

The database tables that should be made available are defined in the tables section of the cloak config. It should be a JSON object that looks as follows:

```
"tables": {
  table_name_1: {
    "db_name": db_name_1,
    "user_id": user_id_column,
    "ignore_unsupported_types": true_or_false
  },
  table_name_2: ...
}
```

The `table_name_x` is the name the table will be available under when querying the data source through Aircloak. The `db_name_x` is the name of the table in the underlying database. In most cases you can use the same name, but the distinction allows some special scenarios, such as exposing a table under a simpler name, or exposing the same database table multiple times under different names.

The `user_id` field is the name of the column that uniquely identifies users - the people or entities whose anonymity should be preserved. See also [Projected tables](#projected_tables) section below.

Finally, `ignore_unsupported_types` should be `true` or `false`. If the value is `true`, the cloak will ignore columns of unsupported data types. If this value is `false`, the cloak will refuse to start if there are one or more columns of an unsupported data type.

### <a name="projected_tables"></a>Projected tables

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

### Table sample rate (only for MongoDb)

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

### Data decoding

Some tables might contain data encoded in a way that it is difficult or impossible to work with directly by analysts. The cloak has support for decoding the data before a query is executed. The decoders are set in the table section. Each one will be applied, sequentially, in the specified order. If an error is encountered by a decoder, processing is skipped for that decoder and the input value is returned (for example, if a decoder tries to decrypt a value and it fails, the original value is kept during further processing; this is useful in case the column holds mixed (encrypted and plain-text) values).
Be careful when using this feature and only decode data in the cloak when there is no other alternative, as any query accessing an encoded column will have greatly reduced performance, due to the cloak not being able to offload processing to the database server.

#### Data decryption

The cloak can decrypt columns encrypted using AES-CBC-128. To activate decryption, specify the `aes_cbc_128` decoder, the decryption key and the list of `text` columns that need to be decrypted, like this:

```
tables": {
  "some_table": {
    "decoders": [
      {"method": "aes_cbc_128", "key": "some_decryption_key", "columns": ["some_column_name", ...]},
      ...
    ],
    ...
  },
  ...
}
```

#### Type conversion

The cloak can convert columns from one type into another, exposing the output type in the columns list provided to analysts. Currently, the following type conversion decoders are available: `text_to_integer`, `text_to_real`, `text_to_datetime`, `text_to_date`, `text_to_time`. For example, a conversion of a column from `text` to `integer` would look like this:

```
tables": {
  "some_table": {
    "decoders": [
      {"method": "text_to_integer", "columns": ["some_column_name", ...]},
      ...
    ],
    ...
  },
  ...
}
```

#### Base-64 decoding

The `base64` decoder can convert a `text` column from a Base-64 encoding to plain text.

### Starting the container

Before starting the container, make sure to update the image to the latest version:

```bash
docker pull quay.io/aircloak/cloak:latest
```

If you want to explicitly control which version you're fetching, then provide the explicit version number instead of the `latest` tag:

```bash
docker pull quay.io/aircloak/cloak:17.3.0
```

With this configuration specified, we can start the cloak container as:

```bash
docker run -d --name cloak \
  -v configuration_folder:/runtime_config \
  --restart=unless-stopped \
  quay.io/aircloak/cloak:latest
```

In the command above, you need to replace `configuration_folder` with the full path to the folder where `config.json` is residing. If you want to have explicit control of the cloak version, replace the `latest` tag in the command with the specific version number.

## Configuring data access

If everything is properly setup, the cloak will connect to the air system. However, data sources configured in the cloak are by default not queryable by any user. To configure proper access do the following:

1. Open the air page in your browser.
1. Click on the gear button, and the admin link.
1. Select _Data sources_ in the menu on the left side. The list of data sources will appear in the central area.
1. Click the _Edit_ button for the desired data source.

You will see the list of available user groups, and you can provide query access to each group by ticking the corresponding checkbox. Make sure to click the _Save_ button after you make your changes.

## Troubleshooting

In the case of problems, you can examine logs by running `docker logs cloak` or `docker logs air`, depending on which part of the system you are troubleshooting. In addition, you can enable debug log messages for the cloak component by including `"debug": true` in cloak `config.json` file. You need to restart the component after you change the configuration file.
