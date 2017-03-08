# Installation tutorial

This tutorial describes how to install and configure the Aircloak components.

## General overview

The Aircloak system consists of two components:

- air - the web site which allows analysts to run anonymized queries in a cloak.
- cloak - the anonymizing query engine.

The access to the cloak component should be highly restricted, since this component has complete read access to the sensitive data. The air component doesn't require such privileges, so you can safely run it on another machine, and maybe even in another network, as long as the cloak component can connect to the air server.

To keep things simple, in this tutorial, we'll be running the air and the cloak containers on the same machine. Moreover, we'll use the Aircloak test-only database server image.

## Installation

Before installing components, make sure that the following prerequisites are met:

- Docker 1.11 or higher should be installed on the host machines.
- The user which installs the components is logged into `quay.io` with `docker login` using credentials provided by Aircloak.
- You have your Aircloak provided `customer-token` available.
- PostgreSQL 9.4 or higher is installed on some machine.

### Installing the air component

#### Setting up the air database

Before installing the air component, you need to create the air user and the database on some PostgreSQL server. You can use arbitrary names for the user and the database. The air user requires full privileges to the air database.

#### Configuration

With database in place, we can create the air configuration by running the following commands:

```bash
$ mkdir -p /aircloak/air/config/

$ cat << EOF > /aircloak/air/config/config.json
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
      "name": air_database_name,
      "user": air_user_name,
      "password": air_user_password
    }
  }
EOF
```

The configuration consists of the following settings:

- site secrets (`auth_secret`, `endpoint_key_base`, `api_token_salt` under the `site` key) - These are used to sign and encrypt various data exchanged with the clients. All secrets should consist of at least 64 characters. For example, you can generate a random secret with the following command: `cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1`
- master site password - you will need this password to create the first user in your site
- customer token - this identifies your `air` installation with the central Aircloak systems
- database settings for the air database (host, port, ssl, database name, user name and password)


#### Running the Air container

Once air is properly configured you can start the air container with the following command:

```bash
docker run -d --name air \
 -v /aircloak/air/config:/runtime_config \
 -p desired_port:8080 \
 quay.io/aircloak/air:latest
```

The most important part here is the mapping of the `/aircloak/air/config` folder to the `/runtime_config` folder in the container. This is how you can pass your configuration to the container. At the very least, this folder must contain the `config.json` file.

The air container listens on port 8080 (HTTP). However, it will also serve HTTPS requests on port 8443 if the private key and the certificate are provided in files named `ssl_key.pem` and `ssl_cert.pem` in the configuration folder. In this case, you'll also need to map the port 8443 to the host.

In the command above, you need to replace the `desired_port` with the actual port on which you want you air to be accessible. If everything was properly configured, you should be able to access air on that port, and create the administrator user using the master password provided in the `config.json`. In the case of problems, you can check log with `docker logs air`.

### Configuring the cloak component

Once the air component is setup, we need to create the configuration for the cloak component:

```bash
mkdir -p /aircloak/cloak/config/

cat << EOF > /aircloak/cloak/config/config.json
  {
    "air_site": air_site,
    "salt": salt,
    "data_sources": [
      data_source_1,
      data_source_2,
      ...
    ]
  }
EOF
```

The `air_site` parameter holds the address of the air site it can be in the form of `"ws://air_host_name:port"` or `wss://air_host_name:port`, where `air_host_name` is the address of the machine where air container is running.

The `salt` parameter is used for anonymization purposes. Make sure to create a strongly random secret for this parameter, for example with the following command: `cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1`.

In the `data_sources` section we're specifying databases and tables which need to be open to analysts for querying. Each `data_source_x` is itself a json in the form of:

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
The `marker` parameter is a string which will be included in the display name of your data source. The `driver` parameter can be one of the following: `mongodb`, `postgresql`, `mysql`, `odbc`. Next, you need to specify the database connection parameters.

Finally, you need to specify queryable tables in the `tables` option. This is a JSON object that looks as follows:

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

The `table_name_x` is a string that defines a display name of the table. The table can be accessed through cloak by that name. The `db_name_x` is the name of the table in the underlying database. In most cases you can use the same name, but the distinction allows some special scenarios, such as exposing a table under a simpler name, or exposing the same database table multiple times under different names.

The `user_id` field is the name of the column that uniquely identifies users - the people or entities whose anonymity should be preserved. See also Projected tables section below.

Finally, `ignore_unsupported_types` should be `true` or `false`. If the value is `true`, cloak will ignore the columns of unsupported data types. If this value is `false`, cloak will refuse to start if there's at least one column of an unsupported data type.

#### Projected tables

In some cases a table doesn't have the `user_id` column itself, but is instead related to another table with such column. For example, you could have the table `accounts` which has the `user_id` column, and the table `transactions` which has `account_id` column.

Now the question is how can you set `user_id` column for the `transactions` table. The answer is to set up the table _projection_. In the `transactions` table definition, you need to specify that `user_id` is obtained from the `accounts` table:

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

Here, we're specifying that the `transaction` table is projected to user through the `accounts` table. The `account_id` field of the `transactions` table corresponds to the `id` field of the `accounts` table. As the result, the `transactions` table in the cloak will be queryable by the cloak, and the results will be properly anonymized. As a side-effect, the `transactions` table as seen by cloak users will also contain the `customer_id` column.

### Running the cloak container

With this configuration specified, we can start the cloak container as:

```bash
docker run -d --name cloak \
  -v /aircloak/cloak/config:/runtime_config \
  quay.io/aircloak/cloak:latest
```

Similarly to the air component, we need to map the configuration folder to `/runtime_config`.

Assuming everything was setup properly, the cloak should be visible in the air system. You can open the local air site in your browser, and verify that the data source is displayed in the list of data sources. In the case of problems, you can check cloak log by running `docker logs cloak`.
