# Installation guide

This guide describes how to install and configure the Aircloak components.

## General overview

The Aircloak system consists of two components:

- air - the web site which allows analysts to run anonymized queries in a cloak.
- cloak - the anonymizing query engine.

These components should run on separate machines. In addition, the access to the cloak component should be highly restricted, since this component has complete read access to the sensitive data. The air component does not require such privileges, so you can optionally run it in another network, as long as the cloak component can connect to the air server.

## Installation

Before installing components, make sure that the following prerequisites are met:

- Docker 1.11 or higher is installed on the host machines.
- The user which installs the components is logged into `quay.io` with `docker login` using credentials provided by Aircloak.
- You have your Aircloak provided `customer-token` available.
- A database in a Postgres server running version 9.4 or higher.
- The air component requires at least 2GB of RAM.
- The cloak component requires at least 8GB of RAM. However, for more complex queries on a larger dataset, more memory might be needed.

### Installing the air component

#### Setting up the air database

Before installing the air component, you need to create the air user and the database on some PostgreSQL server. You can use arbitrary names for the user and the database. The air user requires full privileges to the air database.

#### Configuration

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


#### Running the Air container

Once air is properly configured you can start the air container with the following command:

```bash
docker run -d --name air \
 -v configuration_folder:/runtime_config \
 -p desired_http_port:8080 \
 quay.io/aircloak/air:latest
```

In the command above, the `configuration_folder` is the absolute path to the folder where `config.json` is residing.

The `desired_http_port` parameter is the port on which you want to expose HTTP requests. It is also possible to expose air over HTTPS. In this case, you need to store `ssl_key.pem` and `ssl_cert.pem` files in the `configuration_folder`. You will also need to provide `-p desired_https_port:8443` option. In this case, you can optionally omit the HTTP port mapping, if you want to serve traffic only through HTTPS.

If everything was properly configured, you should be able to access air on that port, and create the administrator user using the master password provided in the `config.json`. In the case of problems, you can check the logs with `docker logs air`.

### Configuring the cloak component

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

The `air_site` parameter holds the address of the air site it can be in the form of `"ws://air_host_name:port"` or `wss://air_host_name:port`, where `air_host_name` is the address of the machine where air container is running. You should use the `ws` prefix if air is serving traffic through HTTP protocol, while `wss` should be used for HTTPS protocol.

The `salt` parameter is used for anonymization purposes. Make sure to create a strongly random secret for this parameter, for example with the following command: `cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1`.

The `data_sources` section configures the databases and tables that will be made available to analysts for querying. Each `data_source_x` is itself a json in the form of:

```
{
  "marker": marker,
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
The `marker` parameter is a string which will be included in the id of the data source. The `driver` parameter can be one of the following: `mongodb`, `postgresql`, `mysql`, `odbc`. Next, you need to specify the database connection parameters.

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

The `user_id` field is the name of the column that uniquely identifies users - the people or entities whose anonymity should be preserved. See also Projected tables section below.

Finally, `ignore_unsupported_types` should be `true` or `false`. If the value is `true`, the cloak will ignore columns of unsupported data types. If this value is `false`, the cloak will refuse to start if there are one or more columns of an unsupported data type.

#### Projected tables

In some cases a table does not have the `user_id` column itself, but is instead related to another table with such column. For example, you could have the table `accounts` which has the `user_id` column, and the table `transactions` which has `account_id` column.

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

For MongoDb database, every table is initially scanned to determine the table schema. This can take a long time for larger tables, which might lead to increased cloak startup time. In this case, you can instruct cloak to analyze only a fraction of table data by providing the `sample_rate` option:

```
"tables": {
  "some_table": {
    "sample_rate": sample_rate,
    ...
  },
  ...
}
```

Where `sample_rate` is an integer between 1 and 100, representing a percentage of data which is going to be sampled.

### Running the cloak container

With this configuration specified, we can start the cloak container as:

```bash
docker run -d --name cloak \
  -v configuration_folder:/runtime_config \
  quay.io/aircloak/cloak:latest
```

In the command above, you need to replace `configuration_folder` with the full path to the folder where `config.json` is residing.

Assuming everything was setup properly, the cloak should be visible in the air system. You can open the local air site in your browser, and verify that configured data sources are displayed in the list of data sources. In the case of problems, you can check cloak log by running `docker logs cloak`.
