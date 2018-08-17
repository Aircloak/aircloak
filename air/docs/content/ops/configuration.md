# Overview

Before starting the system, you need to configure the Insights Air and Insights Cloak components. Both components are configured through a file called `config.json`. Each component requires its own `config.json` file which must be placed in a separate folder. In other words, you can't have a single `config.json` for both components. When starting each component, you need to mount the folder containing this file, as explained in the [Installation guide](installation.md).

## Insights Air configuration

The Insights Air configuration needs to provide the following information:

- database connection parameters (required) - see [Database configuration](#database-configuration)
- web site configuration (required) - see [Web site configuration](#web-site-configuration)
- Insights Air PostgreSQL interface parameters (optional) - see [Insights Air Postgresql interface
  configuration](#insights-air-postgresql-interface-configuration)
- LDAP configuration (optional) - see [LDAP configuration](#ldap-configuration)

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
  "ldap": {
    ...
  }
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

The following fields are optional:

- `port` - defaults to 5432
- `ssl` - defaults to true
- `password` - defaults to empty string

### Web site configuration

This part of the configuration is used to configure the web server of the Insights Air component. The shape of this section is as follows:

```
"site": {
  "auth_secret": secret_string,
  "endpoint_key_base": secret_string,
  "api_token_salt": secret_string,
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

### LDAP configuration

Insights Air can be configured to allow users to login with credentials managed in an LDAP directory service. The
`config.json` snippet below shows all possible configuration options along with their default values where applicable.
Note that the `host`, `user_base`, and `group_base` options are required. Options without a default value are indicated
with a `null`.

```
{
  ...
  "ldap": {
    "host": null,
    "port": 389,
    "bind_dn": "",
    "password": "",
    "encryption": "plain",
    "verify_server_certificate": false,
    "ca_certfile": null,
    "client_certfile": null,
    "client_keyfile": null,
    "user_base": null,
    "user_filter": "(objectClass=*)",
    "user_login": "cn",
    "user_name": "cn",
    "group_base": null,
    "group_filter": "(objectClass=*)",
    "group_name": "dn",
    "group_member": "memberUid",
    "group_member_key": "login",
    "user_group": null
  }
}
```

The options have the following meaning:

* `host` - the hostname of the LDAP server.
* `port` - the port on which to connect to the LDAP server. Defaults to 389.
* `bind_dn` - the DN of the user used to read from the LDAP server. We recommend you set up a read-only user for this
  purpose. Defaults to `""`.
* `password` - the password of the user used to read from the LDAP server. You can set both `bind_dn` and `password` to
  `""` to configure anonymous access. Defaults to `""`.
* `encryption` - the type of encryption to use. Possible values are `"plain"` for no encryption, `"ssl"` for regular
  SSL, and `"start_tls"` for StartTLS. Set this to the type of encryption used by your server. We recommend you use
  either `"ssl"` or `"start_tls"`. Defaults to `"plain"`.
* `verify_server_certificate` - set this to `true` to check the certificate of the server for validity. Requires
  `ca_certfile` to be configured. Defaults to `false`.
* `ca_certfile` - the name of the CA certificate file with which to verify the server certificate. Put the certificate
  file in the same folder as `config.json`.
* `client_certfile` - the name of the client certificate file to use when connecting to the server. Put the certificate
  file in the same folder as `config.json`. By default no client certificate is sent.
* `client_keyfile` - the name of the file containing the key to `client_certfile`. Put the key file in the same folder
  as `config.json`.
* `user_base` - the LDAP subtree in which to look for users.
* `user_filter` - an LDAP filter to restrict which users to sync from `user_base`. See
  [the LDAP page on filters](https://ldap.com/ldap-filters/) for more on how to formulate such filters. Defaults to
  `"(objectClass=*)"`, which matches all objects.
* `user_login` - the name of the attribute from which to take the user's login. Note that users are required to have a
  valid login, so if this attribute is empty for an object, it won't be synced as an Insights Air user. Defaults to
  `"cn"`.
* `user_name` - the name of the attribute from which to take the user's name. Defaults to `"cn"`.
* `group_base` - the LDAP subtree in which to look for groups.
* `group_filter` - an LDAP filter to restrict which groups to sync from `group_base`. See
  [the LDAP page on filters](https://ldap.com/ldap-filters/) for more on how to formulate such filters. Defaults to
  `"(objectClass=*)"`, which matches all objects.
* `group_name` - the name of the attribute from which to take the group's name. Note that groups are required to have a
  valid name, so if this attribute is empty for an object, it won't be synced as an Insights Air group. Defaults to
  `"dn"`.
* `group_member` - the name of the attribute on a group object which lists the group's members. Defaults to
  `"memberUid"`.
* `group_member_key` - the user attribute which will be listed in group objects under `group_member`. Possible values
  are `"login"` and `"dn"`. Defaults to `"login"`.
* `user_group` - the name of the attribute on a user object which lists the groups the user belongs to. The attribute is
  expected to contain the DNs of the groups.

If a valid LDAP configuration is present, Insights Air will periodically sync with the LDAP server to update the list of
users and groups. The syncs will occur immediately after Insights Air starts and every hour after that.

The users and groups created in this way can only be managed in LDAP. That is, their details such as user logins, user
names, and group names cannot be altered through the Insights Air interface. Furthermore, group membership can also only
be altered through LDAP. The only property that can be modified through the Insights Air interface is the list of data
sources available to a given group.

The users synchronized from LDAP will be able to login using their LDAP password and the login configured with
`user_login`. They cannot login using an Insights Air-specific password nor can they change their password via Insights
Air.

When a user is removed from LDAP they will be disabled in Insights Air during the next sync. Only then can the user be
removed from Insights Air. Note that if a user with the same LDAP DN appears again in LDAP then the user will be enabled
and synchronized with this new user. Users who do not match the `user_filter` are treated as non-existent, so you can
disable users by adjusting that filter.

When a group is removed from LDAP that group will be deleted in Insights Air during the next sync.

#### Examples

If your LDAP data looks something like this:

```
dn: ou=users,dc=example,dc=org
objectClass: organizationalUnit

dn: cn=alice,ou=users,dc=example,dc=org
objectClass: simpleSecurityObject
objectClass: organizationalRole
cn: alice
description: Alice Liddell

dn: ou=groups,dc=example,dc=org
objectClass: organizationalUnit

dn: cn=analysts,ou=groups,dc=example,dc=org
objectClass: posixGroup
cn: analysts
description: Wonderland Analysts
memberUid: alice
```

You might have the following LDAP configuration:

```
{
  ...
  "ldap": {
    "host": "ldap.example.org",
    "user_base": "ou=users,dc=example,dc=org",
    "user_login": "cn",
    "user_name": "description",
    "group_base": "ou=groups,dc=example,dc=org",
    "group_name": "description",
    "group_member": "memberUid",
    "group_member_key": "login"
  }
}
```

Your group membership might be specified in user attributes instead:

```
dn: ou=users,dc=example,dc=org
objectClass: organizationalUnit

dn: cn=alice,ou=users,dc=example,dc=org
objectClass: simpleSecurityObject
objectClass: organizationalRole
cn: alice
description: Alice Liddell
group: cn=analysts,ou=groups,dc=example,dc=org

dn: ou=groups,dc=example,dc=org
objectClass: organizationalUnit

dn: cn=analysts,ou=groups,dc=example,dc=org
objectClass: posixGroup
cn: analysts
description: Wonderland Analysts
```

In that case you'd use a configuration like this:

```
{
  ...
  "ldap": {
    "host": "ldap.example.org",
    "user_base": "ou=users,dc=example,dc=org",
    "user_login": "cn",
    "user_name": "description",
    "group_base": "ou=groups,dc=example,dc=org",
    "group_name": "description",
    "user_group": "group"
  }
}
```

## Insights Cloak configuration

The Insights Cloak configuration is used to provide the following information:

- URL where the Insights Air component can be reached
- Anonymization salt
- Data sources which can be queried - see [Data source configuration](#data-source-configuration)

The general shape of `config.json` is:

```
{
  "air_site": string,
  "salt": string,
  "concurrency": integer,
  "data_sources": string
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

The `concurrency` field is optional and controls the amount of additional threads used for processing the selected data.
The default setting is 0, which means a single thread processes the data coming in from the database server. For small
data sets, this is usually sufficient, but for bigger data sets, this might turn out to be a bottleneck during query
execution. By increasing this value (to 2 or 4 is recommended), additional threads will be used when ingesting the data,
executing the query faster, but also consuming more memory.

### Data source configuration

The `data_sources` parameter should give the path to subfolder within the folder where the Insights Cloak config is stored that contains the datasource configurations.

Each datasource configuration should be in JSON format and put in an individual file with the `.json` extension.
The configuration takes the following form:

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
  "concurrency": integer,
  "tables": tables
}
```

The `name` parameter is a string which will be used to identify the data source throughout the Insights Air interface and APIs.

The `driver` parameter can be one of the following: `mongodb`, `postgresql`, `mysql`, `sqlserver`, `saphana`, `sapiq`. The `parameters` json, then specifies the database connection parameters.

Some of these drivers use ODBC protocol to talk to the database. These drivers are `sqlserver`, `saphana`, and `sapiq`. Since they rely on ODBC, they accept some additional connection parameters:

  - `encoding` which has possible values of "latin1", "unicode", "utf8", "utf16", "utf32", "utf16-big", "utf16-little", "utf32-big", "utf32-little".
  - `odbc_parameters` - ODBC specific parameters for the ODBC driver which is used to talk to the database.

These parameters are optional, and are only required for particular installations, where the default values will not suffice.

The `concurrency` field is optional and controls the amount of additional threads used for processing the selected data.
If not present, the global setting is used.

The database tables that should be made available for querying are defined in the `tables` section of the cloak config. The value of the `tables` key is a JSON object that looks as follows:

```
"tables": {
  table_name_1: {
    "db_name" | "query": string,
    "user_id": string
  },
  table_name_2: ...
}
```

Each `table_name_x` key specifies the name the table will be available under when querying the data source through Aircloak.

The `user_id` field is the name of the column that uniquely identifies users - the people or entities whose anonymity
should be preserved. If this field is set to `null` the table will be classified as not containing data requiring
anonymization. Queries over such tables are not subject to the anonymization restrictions, filters and aggregations,
and *no attempts will be made to anonymize the data the table contains!*

The database table can be declared by either using `db_name` or as an SQL view using `query`.
These options are mutually exclusive.

The `db_name` is the name of the table in the underlying database. In most situations you can use the same name
(in which case the field can be omitted), but the distinction allows some special scenarios, such as exposing
a table under a simpler name, or exposing the same database table multiple times under different names.

If the `query` field is present instead, a virtual table is created, similar to an SQL view. The provided query can gather
data from multiple tables, filter what columns are exposed and pre-process, pre-filter or pre-aggregate the data. The
supported SQL features are the same as in other Aircloak queries, but the anonymization specific restrictions (like
requiring a numerical range to have an upper and lower bound, for example) do not apply.
An example configuration for a virtual table would look like this:

```
table_name: {
  query": "
    SELECT cast(t2.uid as integer), t2.age, t1.*
    FROM t1 INNER JOIN t2 ON t1.pk = t2.fk
    WHERE t2.age > 18
  ",
  "user_id": "uid"
}
```

The query can only select data from existing tables (or views) in the source database (it can not reference other virtual
or projected tables from the configuration file).
If the virtual table contains columns with duplicated names, only the first one is kept and the rest are dropped.
Constant columns are also dropped from the table.

#### Projected tables

In some cases a table does not have a `user_id` column but is related to another table that does. You could for example have an `accounts` table with a `customer_id` column, and a table named `transactions` with an `account_id` column.

To get a `customer_id` column in the `transactions` table, the cloak needs to be configured to derive it from the `accounts` table:

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

The extra column added to the table has the name of the user id column in the related table. If the name clashes with
a column that already exists in the table, you can rename it using the `user_id_alias` option.
As an example let's assume you have two tables: `users` and `purchases`. The `users` table has a column `uuid` that
should be used as the user identifier, as well as an `id` column used to refer to a specific user in the context of the
database. The `purchases` table has a `users_fk_id` column referring to the `id` column of the `users` table, as well as
a `uuid` column uniquely identifying a purchase. Unless otherwise specified, the column added by Aircloak would be called
`uuid` after the user id column in the `users` table. This would clash with the `uuid` column already existing in the
`purchases` table. You get around this using the `user_id_alias` option.

```
"tables": {
  "users": {
    "db_name": "users",
    "user_id": "uuid"
  },
  "purchases": {
    "db_name": "purchases",
    "projection": {
      "table": "users",
      "foreign_key": "users_fk_id",
      "primary_key": "id",
      "user_id_alias": "user_id"
    }
  },
  ...
}
```

Given the above configuration a column named `user_id` would be added to the `purchases` table, rather than a column
called `uuid`.

Projected tables are translated internally into virtual tables. Projected tables can not reference any virtual tables.

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

#### Manually classifying isolating columns

Insights Cloak can automatically detect whether a column isolates users or not.  For large database tables this check
can be slow and resource-intensive. An administrator may choose to manually configure whether a given column isolates
users or not, removing the need for automated classification.

How Insights Cloak handles classifying columns is configured for each table individually, using
`auto_isolating_column_classification` (defaulting to true) and `isolating_columns` (empty by default).
`isolating_columns` is a dictionary where each key is the name of a column, and the value of `true` or `false` indicates
if the column should be considered as isolating users or not. The behaviour for columns not included in that dictionary
is guided by `auto_isolating_column_classification` - if it's set to `true`, then Insights Cloak will try to compute
if the column is isolating as normal, if to `false` then it will assume it's isolating.

Take this example:

```
{
  tables: {
    regular_table: {
      db_name: "regular_table"
    },

    auto_table: {
      db_name: "auto_table",
      auto_isolating_column_classification: true,
      isolating_columns: {"telephone_number": true, "first_name": false}
    },

    manual_table: {
      db_name: "manual_table",
      auto_isolating_column_classification: false,
      isolating_columns: {"first_name": false}
    }
  }
}
```

In this case Insights Cloak will automatically compute which columns in `regular_table` are isolating. For `auto_table`
it will treat `telephone_number` as isolating, `first_name` as not isolating, and automatically handle all other
columns. `manual_table` has the automatic isolating column detection turned off. All columns that have not been manually
classified will therefore be treated as if they isolate users.

__Warning__ The safest option is to treat a column as isolating. Manually classifying a column as not isolating may lead
to privacy loss. It is safe to classify columns as not isolating only when sure that most values in that column appear
for multiple users. Please contact [support@aircloak.com](mailto:support@aircloak.com) if you need help classifying your
data.

#### Tips and tricks

It is common to have multiple Insights Cloak instances sharing the same datasource definitions.
Maintaining separate copies of the datasource definition for each Insights Cloak instance complicates maintenance,
as you will have to update multiple copies of files if a datasource definition changes.

The recommended, and common, solution to this problem is to create a single copy of the datasource configuration
files and symlink these into the configuration folders of the individual Insights Cloak instances.

Here is an example of how one can do this in pratice:

Let's consider a scenario where we have a shared folder where we store all datasource definitions. It is called `data_sources_available`.
In the configuration folder of each Insights Cloak instance we create a folder called `data_sources_enabled`. For each datasource we want
to enable for a given Insights Cloak instance we create a symlink from the `data_sources_enabled` folder to the
datasource definition stored in the `data_sources_available` folder.

If we have two Insights Cloak instances, the folder structure would look like this:

```
$ tree configs

configs/
├── data_sources_available
│   ├── data_source1.json
│   └── data_source2.json
├── insights-cloak1
│   ├── config.json
│   └── data_sources_enabled
├── insights-cloak2
│   ├── config.json
│   └── data_sources_enabled
```

In order to have `insights-cloak1` serve `data_source1`, and `data_source2`, and `insights-cloak2` server `data_source1`,
we would create the following symlinks:

```
$ cd config/insights-cloak1/data_sources_enabled/
$ ln -s ../../data_sources_available/data_source1.json data_source1.json
$ ln -s ../../data_sources_available/data_source2.json data_source2.json
$ cd ../../insights-cloak2/data_sources_enabled
$ ln -s ../../data_sources_available/data_source1.json data_source1.json
```

The resulting file structure would then look as follows.

```
$ tree configs

configs/
├── data_sources_available
│   ├── data_source1.json
│   └── data_source2.json
├── insights-cloak1
│   ├── config.json
│   └── data_sources_enabled
│       └── data_source1.json -> ../../data_sources_available/data_source1.json
│       └── data_source2.json -> ../../data_sources_available/data_source2.json
├── insights-cloak2
│   ├── config.json
│   └── data_sources_enabled
│       └── data_source1.json -> ../../data_sources_available/data_source1.json
```

Enabling or disabling further datasources for individual Insights Cloak instances is then only a matter of adding or
removing a symlink.

##### Hiding columns

In some cases, it might not be desirable that all of the columns in a table are exposed to analysts.
Virtual tables can be used to hide one or more columns, by explicitly listing only the columns that are valid for querying.

For example, assuming we have the table `t` with columns `uid, x, y, z`, and we wish to hide column `y` from analysts,
the following configuration file will do the trick:

```
t: {
  query": "SELECT uid, x, z FROM t",
  "user_id": "uid"
}
```

Alternatively, if the target table has a large number of columns and we don't want to list all of them, we can explicitly
select constants with the hidden columns' names, then star-select everything else. This uses the fact that duplicated
columns are eliminated by only keeping the first instance and then that constants are dropped from the list of exposed
columns. For example:

```
t: {
  query": "SELECT 0 AS y, t.* FROM t",
  "user_id": "uid"
}
```
