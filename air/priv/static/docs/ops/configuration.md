# Overview

Before starting the system, you need to configure the Insights Air and Insights Cloak components. Both components are configured through a file called `config.json`. Each component requires its own `config.json` file which must be placed in a separate folder. In other words, you can't have a single `config.json` for both components. When starting each component, you need to mount the folder containing this file, as explained in the [Installation guide](/ops/installation.md).

## Insights Air configuration

The Insights Air configuration needs to provide the following information:

- database connection parameters (required) - see [Database configuration](#database-configuration)
- web site configuration (required) - see [Web site configuration](#web-site-configuration)
- Insights Air PostgreSQL interface parameters (optional) - see [Insights Air Postgresql interface
  configuration](#insights-air-postgresql-interface-configuration)
- LDAP configuration (optional) - see [LDAP configuration](#ldap-configuration)
- Configuration for connecting to Diffix Explorer (optional) - see [Diffix Explorer configuration](#diffix-explorer-configuration)

The general shape of `config.json` is therefore:

```
{
  "name": string,
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
  },
  "explorer": {
    ...
  }
}
```

The `name` property is used to uniquely identify the air instance in the system. If you're running multiple instances, make sure to give each instance a unique name.

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

This part of the configuration is used to configure the web server of the Insights Air component. The shape of this
section is as follows:

```
"site": {
  "auth_secret": secret_string,
  "endpoint_key_base": secret_string,
  "endpoint_public_url": string,
  "cloak_secret": secret_string,
  "master_password": string,
  "certfile": string,
  "keyfile": string,
  "privacy_policy_file": string,
  "license_file": string,
  "users_and_datasources_file": string,
  "browser_long_polling": boolean
},
```

In the snippet above, the type `secret_string` indicates a string which should consist of at least 64 characters. The corresponding parameters are used to sign and encrypt various data. Make sure to choose values which are random enough, or otherwise the security of the system might be compromised. For example, to generate a random secret, you can use the following command:

```
cat /dev/urandom |
  LC_ALL=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```

Of the parameters above, the only required ones are the `auth_secret` and `endpoint_key_base` parameters, as well as one
of `master_password` or `users_and_datasources_file`. The other parameters such as the `privacy_policy_file`,
`users_and_datasources_file`, and `license_file` all specify values that can also be configured in the
Insights Air web interface.
These parameters can be used to fully configure a system ahead of time. This is useful when performing automated
deployments. For more information on ahead of time configuration, please read the [ahead of time
configuration](/ops/ahead-of-time-configuration.md) guide.

The `master_password` parameter specifies the password (in clear text) which is required when creating the first administrator
in the Insights Air web interface. If you attempt to access the Insights Air interface while no administrative user has been setup,
you will be prompted to create one. To do so you have to type in the `master_password` the system is configured with.
This password will no longer be needed once the first administrator has been created.

The `endpoint_public_url` should be the full root URL that the Insights Air instance is accessible on the internet. It should be the address you would go to when visiting Insights Air using the browser. In other words, it should also include `http://` or `https://`. This parameter is used to generate correct URLs.

The `cloak_secret` setting is optional. If not set (default) all Insights Cloak instances will be allowed to connect to
the Insights Air instance. If set, then only instances with the same `cloak_secret` set in their configuration files
will be allowed. See [Insights Cloak configuration](#insights-cloak-configuration) for more.

The final two parameters `certfile` and `keyfile` are optional. They are used to specify the certificate and key for the HTTPS interface. If these parameters are provided, you will also need to put the corresponding files in the same folder as the `config.json` file. Once you do that, the site will accept HTTPS traffic as well as HTTP traffic. If you omit these parameters, the site will only accept HTTP traffic.

The ports on which the site will listen are hardcoded. HTTP traffic is served via port 8080, while HTTPS is served via 8443. As explained in the [Installation guide](/ops/installation.md#insights-air), you can use the Docker port mapping option to decide under which port numbers you want to expose these endpoints on the host server.

We strongly suggest only exposing the Insights Air interface to clients using HTTPS. You might want to terminate the SSL connection at a reverse proxy such as [nginx](https://nginx.org/) or [apache](https://httpd.apache.org/), or alternatively make use of the HTTPS server offered as part of Insights Air.

By default, when Insights Air is accessed from the browser, a websocket connection is established. This connection is used to push real-time notifications in various situations. If Insights Air is behind a proxy, and you don't want to allow forwarding of websocket connections, you can explicitly force the long polling protocol. This can be done by setting the `browser_long_polling` option to `true`.

### Insights Air PostgreSQL interface configuration

This part of the configuration allows you to instruct the Insights Air component to accept requests over the PostgreSQL wire protocol. If this is configured, Insights Air can be queried from client applications which understand this protocol, such as Tableau.

The configuration consists of the following parameters:

```
"psql_server": {
  "require_ssl": boolean,
  "certfile": string,
  "keyfile": string,
  "max_connections": positive_integer
}
```

The `require_ssl` parameter specifies whether the connection requires all clients to connect over SSL. If this value is `true`, you also need to provide `certfile` and `keyfile` parameters which specify the file names of the certificate and the key. These files need to be placed in the same folder as the `config.json` file.

If `require_ssl` is false, then the server will accept TCP connection as well as SSL. However, if `certfile` and `keyfile` parameters are not provided, then the server will only work with unencrypted TCP connections.

Regardless of which transport protocol(s) are allowed, the server will always accept requests on the port 8432. As explained in the [Installation guide](/ops/installation.md#insights-air), you can use the docker port mapping to expose this port to the outside world.

Once the component is started, you can test the connectivity with the `psql` command line tool:

```
psql -h insights_air_ip_address -p postgresql_interface_port -d data_source_name -U user_name
```

Where `postgresql_interface_port` is the PostgreSQL interface port provided when the component is started, as explained in the [Installation Guide](/ops/installation.md#insights-air).

In order for the above command to work, the cloak component must be started as well, and the user must have permissions to query the given data source.

The `max_connections` property can be used to configure the maximum allowed number of simultaneously open connections. The incoming connections which would cause the limit to be exceeded are immediately closed. This property is optional, and if not provided, the default value of 1024 is used.

### LDAP configuration

Insights Air can be configured to allow users to login with credentials managed in an LDAP directory service. Note that
this feature is licensed separately. If you would like to add LDAP sync to your license, contact
[support@aircloak.com](mailto:support@aircloak.com). The `config.json` snippet below shows all possible configuration
options along with their default values where applicable. Note that the `host`, `user_base`, and `group_base` options
are required. Options without a default value
are indicated with a `null`.

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

- `host` - the hostname of the LDAP server.
- `port` - the port on which to connect to the LDAP server. Defaults to 389.
- `bind_dn` - the DN of the user used to read from the LDAP server. We recommend you set up a read-only user for this
  purpose. Defaults to `""`.
- `password` - the password of the user used to read from the LDAP server. You can set both `bind_dn` and `password` to
  `""` to configure anonymous access. Defaults to `""`.
- `encryption` - the type of encryption to use. Possible values are `"plain"` for no encryption, `"ssl"` for regular
  SSL, and `"start_tls"` for StartTLS. Set this to the type of encryption used by your server. We recommend you use
  either `"ssl"` or `"start_tls"`. Defaults to `"plain"`.
- `verify_server_certificate` - set this to `true` to check the certificate of the server for validity. Requires
  `ca_certfile` to be configured. Defaults to `false`.
- `ca_certfile` - the name of the CA certificate file with which to verify the server certificate. Put the certificate
  file in the same folder as `config.json`.
- `client_certfile` - the name of the client certificate file to use when connecting to the server. Put the certificate
  file in the same folder as `config.json`. By default no client certificate is sent.
- `client_keyfile` - the name of the file containing the key to `client_certfile`. Put the key file in the same folder
  as `config.json`.
- `user_base` - the LDAP subtree in which to look for users.
- `user_filter` - an LDAP filter to restrict which users to sync from `user_base`. See
  [the LDAP page on filters](https://ldap.com/ldap-filters/) for more on how to formulate such filters. Defaults to
  `"(objectClass=*)"`, which matches all objects.
- `user_login` - the name of the attribute from which to take the user's login. Note that users are required to have a
  valid login, so if this attribute is empty for an object, it won't be synced as an Insights Air user. Defaults to
  `"cn"`.
- `user_name` - the name of the attribute from which to take the user's name. Defaults to `"cn"`.
- `group_base` - the LDAP subtree in which to look for groups.
- `group_filter` - an LDAP filter to restrict which groups to sync from `group_base`. See
  [the LDAP page on filters](https://ldap.com/ldap-filters/) for more on how to formulate such filters. Defaults to
  `"(objectClass=*)"`, which matches all objects.
- `group_name` - the name of the attribute from which to take the group's name. Note that groups are required to have a
  valid name, so if this attribute is empty for an object, it won't be synced as an Insights Air group. Defaults to
  `"dn"`.
- `group_member` - the name of the attribute on a group object which lists the group's members. Defaults to
  `"memberUid"`.
- `group_member_key` - the user attribute which will be listed in group objects under `group_member`. Possible values
  are `"login"` and `"dn"`. Defaults to `"login"`.
- `user_group` - the name of the attribute on a user object which lists the groups the user belongs to. The attribute is
  expected to contain the DNs of the groups.

If a valid LDAP configuration is present, Insights Air will periodically sync with the LDAP server to update the list of
users and groups. The syncs will occur immediately after Insights Air starts and every hour after that. You can also
trigger a sync manually by going to `Admin -> Users` or `Admin -> Groups` and clicking `Sync Now` next to the LDAP
section.

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

### Diffix Explorer Configuration

The Diffix Explorer integration is optional. You can activate it by including the `explorer` parameter in your
configuration. It specifies the Diffix Explorer instance Insights Air will connect to. The configuration looks like
this:

```
"explorer": {
  "url": string
}
```

The single property `url` is the URL where Insights Air can find a running version of Diffix Explorer.
Note that if your Diffix Explorer instance is running behind a reverse proxy that sends an HTTP redirect (status code
301 or equivalent) then the Diffix Explorer integration will fail. Please use the URL being redirect to instead. This
includes if your reverse proxy redirects from HTTP to HTTPS. In the latter case, please explicitly include `https://`
in the URL. For the Diffix Explorer integration to work properly, you will also need to configure the
[`site.endpoint_public_url`](#web-site-configuration) setting.

In the admin control panel you can choose which tables Diffix Explorer should analyze – by default none are.

Only tables meeting the following criteria can be analyzed:

- the table must contain a `user-id` column, and
- the table must have a least one other column that is selectable (i.e. not marked as unselectable) and not isolating

For more information about selectable and unselectable columns, please have a look at the
[section on configuring tables in data sources](/ops/configuration?id=tables).
For more information about isolating columns, [read the section that describes what they are and how they can be configured](/sql/restrictions?id=isolating-columns).

Some of Diffix Explorer's behaviors can only be configured through the use of environment variables.
For example, in order to limit the number of parallel queries issued, you can use the `Explorer__MaxConcurrentQueries` environment variable.

A full overview of the configuration variables can be found in the [project documentation](https://github.com/diffix/explorer/#configuration).

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
  "cloak_secret": string,
  "data_sources": string,
  "concurrency": integer,
  "lcf_buckets_aggregation_limit": integer,
  "max_parallel_queries": positive_integer,
  "allow_any_value_in_when_clauses": boolean,
  "allow_any_value_in_in_clauses": boolean,
  "connection_timeouts": {
    "idle": integer,
    "connect": integer,
    "request": integer
  },
  "analysis_queries": {
    "concurrency": positive_integer,
    "time_between_queries": integer,
    "minimum_memory_required": number
  }
}
```

The `air_site` parameter holds the URL where Insights Air component can be reached. It can be in the form of `"ws://air_host_name:port"` or `"wss://air_host_name:port"`, where `air_host_name` is the address of the machine where the Insights Air component is running. You should use the `ws` prefix if Insights Air is serving traffic over HTTP, while `wss` should be used for the HTTPS protocol.

The `salt` parameter is used for anonymization purposes. If your Aircloak Insights installation has multiple Insights Cloak instances
you must make sure they use the same salt. Failing to do so has a negative impact on the quality of the anonymization.
You can derive a strong `salt` parameter using a command such as:

```
cat /dev/urandom |
  LC_ALL=C tr -dc 'a-zA-Z0-9' |
  fold -w 64 |
  head -n 1
```

The `cloak_secret` setting is used to authenticate the Insights Cloak instance when connecting to Insights Air. It is
required only if `cloak_secret` was configured in Insights Air (see [Web site configuration](#web-site-configuration)),
and in that case it needs to be set to the same value.

The `concurrency` field is optional and controls the amount of additional threads used for processing the selected data.
The default setting is 0, which means a single thread processes the data coming in from the database server. For small
data sets, this is usually sufficient, but for bigger data sets, this might turn out to be a bottleneck during query
execution. By increasing this value (to 2 or 4 is recommended), additional threads will be used when ingesting the data,
executing the query faster, but also consuming more memory. This setting can be overridden per data-source.

The `lcf_buckets_aggregation_limit` is optional and controls the maximum number of columns for which partial aggregation
of low-count filtered rows is done. The default value is 3. This setting can be overridden per data-source. More details
can be found in the [Low-count filtering](/sql/query-results.md#low-count-filtering) section.

The `max_parallel_queries` field is optional and controls the maximum number of queries that the cloak will run
simultaneously. The default value is 10.

The `allow_any_value_in_when_clauses` field is optional and controls whether restricted `WHEN` clauses are allowed or
not to use any value in anonymizing queries. The default value is false, which means only frequent values are permitted.

The `allow_any_value_in_in_clauses` field is optional and controls whether restricted `IN` clauses are allowed or
not to use any value in anonymizing queries. The default value is false, which means only frequent values are permitted.

The `connection_timeouts` field is optional and it controls various database connection timeouts.

The `connection_timeouts.idle` field is optional and it determines how many seconds idle database connections are kept
before they are closed. It needs to be an integer value between 1 and 86400 (1 day). If not set, a default timeout
value of 60 seconds (1 minute) is used.

The `connection_timeouts.connect` field is optional and it determines how many seconds the Insights Cloak waits for a
database connection to be established. It needs to be an integer value between 1 and 3600 (1 hour). If not set, a
default timeout value of 5 seconds is used.

The `connection_timeouts.request` field is optional and it determines how many seconds the Insights Cloak waits for a
database request to complete. It needs to be an integer value between 1 and 86400 (1 day). If not set, a default
timeout value of 43200 seconds (12 hours) is used.

The `analysis_queries` field is optional and controls the rate of [column analysis](/sql/restrictions.md#column-analysis) queries.
If Insights Cloak exhausts too many system resources while analyzing columns, then you can specify these parameters to reduce overall load.

The `analysis_queries.concurrency` field is optional and controls the number of maximum concurrent analysis queries issued by Insights Cloak.
It needs to be an integer value between 1 and 3. The default value is 3.

The `analysis_queries.time_between_queries` field is optional and it determines how many milliseconds to wait before issuing further analysis queries.
This waiting period can be useful to allow Insights Cloak and databases to release resources before handling subsequent requests.
It needs to be a non-negative integer value. If not set, a default value of 0 is used, meaning no delay between queries.

The `analysis_queries.minimum_memory_required` field is optional and controls the minimum relative system memory required to run analysis queries.
If available memory falls below this threshold, then further analysis queries will be suspended until more memory becomes available.
Requires a decimal value between 0 and 1.
For example, a value of 0.3 specifies that analysis queries will not run when available memory is under 30%.
If not set, a default value of 0 is used, meaning analysis queries will run even when the system is low on memory.

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
  "lcf_buckets_aggregation_limit": integer,
  "max_rare_negative_conditions": integer,
  "analyst_tables_enabled": boolean,
  "tables": tables,
  "load_comments": boolean
}
```

The `name` parameter is a string which will be used to identify the data source throughout the Insights Air interface and APIs.

The `driver` parameter can be one of the following: `postgresql`, `mysql`, `sqlserver`, `oracle`. The `parameters` json, then specifies the database connection parameters.

Some of these drivers use the ODBC protocol to talk to the database. These drivers are `sqlserver`, and `oracle`.
Since they rely on ODBC, they accept some additional connection parameters:

- `encoding` which has possible values of "latin1", "unicode", "utf8", "utf16", "utf32", "utf16-big", "utf16-little", "utf32-big", "utf32-little".
- `odbc_parameters` - ODBC-specific parameters for the ODBC driver which is used to talk to the database.

These parameters are optional, and are only required for particular installations, where the default values do not suffice.

The `concurrency` field is optional and controls the amount of additional threads used for processing the selected data.
If not present, the global setting is used.

The `lcf_buckets_aggregation_limit` field is optional and controls the maximum number of columns for which partial
aggregation of low-count filtered rows is done. If not present, the global setting is used.

The `max_rare_negative_conditions` affects how many negative conditions containing rare values are allowed per
anonymizing query. It defaults to a safe value of 0, which rejects all rare negative conditions, and should,
under most circumstances, not be altered.
Increasing the value above the default should only be done if it has been deemed safe.

The `analyst_tables_enabled` can be set to true to enable creation of analyst tables. By default, this parameter is set to false. See the [Analyst tables](#analyst-tables) section for more details.

The `load_comments` flag indicates whether database level comments should be loaded from configured tables during data source initialization.
Defaults to `true`, meaning comments will be loaded.

### Tables

The database tables that should be made available for querying are defined in the `tables` section of the cloak config. The value of the `tables` key is a JSON object that looks as follows:

```
"tables": {
  "table_name_1": {
    "db_name" | "query": string,
    "content_type": "personal" | "non-personal",
    "keys": [{"key_type_1": "column_name_1"}, ...],
    "exclude_columns": ["column1", "column2", ...],
    "unselectable_columns": ["column1", "column2", ...],
    "comments": {
      "table": "Comment on table 1.",
      "columns": {
        "column1": "Comment on column 1",
        "column2": "Comment on column 2"
      }
    }
  },
  "table_name_2": ...
}
```

Each `table_name_x` key specifies the name the table will be available under when querying the data source through Aircloak.

The `content_type` is an optional field which determines whether the data in the table is sensitive or not. It can have one
of the following values: `personal` (default) and `non-personal`. Tables with data about individuals or entities whose
anonymity should be preserved must be marked with the content type `personal`. If any such table is included in a query, the
query will underlie the anonymization restrictions applied by Aircloak Insights and produce anonymized results. If the
content type field is set to `non-personal`, the table will be classified as not containing data requiring anonymization.
Queries over such tables are not subject to the anonymization restrictions. _No attempts will be made to anonymize the data
they contain!_

The database table can be declared by either using `db_name` or as an SQL view using `query`.
These options are mutually exclusive.

The `db_name` is the name of the table in the underlying database. In most situations you can use the same name
(in which case the field can be omitted), but the distinction allows some special scenarios, such as exposing
a table under a simpler name, or exposing the same database table multiple times under different names. See the [Referencing database tables](#referencing-database-tables) section for details.

If the `query` field is present instead, a virtual table is created, similar to an SQL view. The provided query can gather
data from multiple tables, filter what columns are exposed and pre-process, pre-filter or pre-aggregate the data. The
supported SQL features are the same as in other Aircloak queries, but the anonymization-specific restrictions (like
requiring a numerical range to have an upper and lower bound, for example) do not apply.
An example configuration for a virtual table would look like this:

```
"table_name": {
  "query": "
    SELECT cast(t2.uid as integer), t2.age, t1.*
    FROM t1 INNER JOIN t2 ON t1.pk = t2.fk
    WHERE t2.age > 18
  ",
  "keys": [
    {"user_id": "id"}
  ]
}
```

The query can only select data from existing tables (or views) in the source database (it can not reference other virtual
or projected tables from the configuration file).
If the virtual table contains columns with duplicated names, only the first one is kept and the rest are dropped.
Constant columns are also dropped from the table.

The `exclude_columns` is an optional parameter. It takes the form of an array and specifies columns to exclude from the underlying table.
Excluded columns will not appear in the data source and cannot be referenced in any way from queries.

The `unselectable_columns` is an optional parameter for personal tables.
It takes the form of an array and marks columns as unselectable.
Unselectable columns can only be joined together, counted, and/or grouped by.

The `comments` field is optional and can be used to attach a description to tables and columns.
Comments are visible in the Insights Air interface and are also returned from `SHOW` statements.
Database-level comments are automatically retrieved and attached to tables.

##### Keys

Entities in a dataset, whether they be persons, transactions, or products, are usually identifiable by a single column
value. This could be a user, patient or customer id in the case of a person, a transaction id for a transaction or a product
id for a product. We call these types of identifiers _keys_. When you configure your tables you need to mark these columns
as keys and declare what type of entity they describe.

When querying tables containing personal data it is a requirement that at least one of the tables queried contains a key of
type `user_id`. Other tables that are part of the query need to be joined with a table containing a `user_id` key via the
pre-configured key-relationships.

The following restrictions are currently in place when configuring keys:

- A column can have one key tag at the most.
- A `personal` table can have at most one `user_id` key.
- A `non-personal` table can't have any `user_id` keys.

An example configuration file for a database containing information about customers, accounts, transactions and bought
products might look like this:

```
"tables": {
  "customers": {
    "keys": [
      {"user_id": "id"}
    ]
  },
  "accounts": {
    "keys": [
      {"user_id": "customer_id"},
      {"account_id_key": "id"}
    ]
  },
  "transactions": {
    "keys": [
      {"account_id_key": "account_id"},
      {"product_id_key": "product_id"}
    ]
  },
  "products": {
    "content_type": "non-personal",
    "keys": [
      {"product_id_key": "id"}
    ]
  }
}
```

while a valid query that accesses all tables might look like this:

```sql
SELECT
  customer.job,
  AVG(transaction.price)
FROM
  customer
  INNER JOIN accounts ON customer.id = accounts.customer_id
  INNER JOIN transactions ON accounts.id = transactions.account_id
  INNER JOIN products ON transactions.product_id = products.id
WHERE products.type = 'car'
GROUP BY 1
```

##### Referencing database tables

Database tables are referenced when providing the `db_name` property. They can also be referenced in the query of virtual tables. The rules explained here are the same for both cases.

When referencing a database table, two characters are considered as special: the dot character (`.`) and the double quote character (`"`). If the name contains any of these characters, the name has to be quoted inside double quotes. Since the JSON string is already quoted inside double quotes, you need to use the `\"` syntax:

```
"db_name": "\"some.table\""
```

If the `"` character is a part of the table name, you need to quote the table name, and provide the double quote as `\"\"` inside the quoted name. For example, if the table name is `some"table`, you can specify it as:

```
"db_name": "\"some\"\"table\""
```

In some cases you might need to specify a fully qualified name, for example to provide a different database schema. In this case, you need to separate different parts with the dot character:

```
"db_name": "some_schema.some_table"
```

When quoting a multi-part identifier, you need to quote each part separately. For example, if the schema name is `some.schema`, and the table name is `some.table`, you can specify it as follows:

```
"db_name": "\"some.schema\".\"some.table\""
```

Also note that you only need to quote the part which contains special characters. In the following example, we quote the schema name (because it contains the dot character), but not the table name (because it doesn't contain any special characters).

```
"db_name": "\"some.schema\".some_table"
```

However, it's not an error if you quote each part, regardless of whether it requires quoting or not.

It's also worth mentioning that `db_name` is case sensitive, irrespective of whether it's quoted or not. Therefore, you should use the exact capitalization of the underlying database.

For example, let's say that the table is created with the following statement:

```
create table user_data(uid integer, ...)
```

In PostgreSQL, the table name will be lower-cased, while in Oracle, it will be upper-cased. Therefore, when providing `db_name`, you should specify `"user_data"` if the data source is a PostgreSQL database, or `"USER_DATA"` if the data source is an Oracle database.

Of course, if you explicitly used a non-default capitalization, then you need to use the same capitalization when specifying the `db_name`. For example, let's say that the following create statement was used to create a PostgreSQL table:

```
create table "UserData"(uid integer, ...)
```

In this case, you need to provide `"UserData"` as the `db_name` property.

##### Manually classifying isolating columns

Insights Cloak can automatically detect whether a column isolates users or not. For large database tables this check
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
  "tables": {
    "regular_table": {
      "db_name": "regular_table"
    },

    "auto_table": {
      "db_name": "auto_table",
      "auto_isolating_column_classification": true,
      "isolating_columns": {"telephone_number": true, "first_name": false}
    },

    "manual_table": {
      "db_name": "manual_table",
      "auto_isolating_column_classification": false,
      "isolating_columns": {"first_name": false}
    }
  }
}
```

In this case Insights Cloak will automatically compute which columns in `regular_table` are isolating. For `auto_table`
it will treat `telephone_number` as isolating, `first_name` as not isolating, and automatically handle all other
columns. `manual_table` has the automatic isolating column detection turned off. All columns that have not been manually
classified will therefore be treated as if they isolate users.

**Warning** The safest option is to treat a column as isolating. Manually classifying a column as not isolating may lead
to privacy loss. It is safe to classify columns as not isolating only when sure that most values in that column appear
for multiple users. Please contact [support@aircloak.com](mailto:support@aircloak.com) if you need help classifying your
data.

##### Column value shadow database

Insights Cloak automatically maintains a cache of column values that occur frequently.
This allows certain anonymization practices to be relaxed when doing so does not cause harm.
The creation of this cache requires a set of database queries to be run against the database
that can become prohibitively expensive for large databases. You can turn of the creation
and maintenance of this shadow database when you either do not need the
[extra capabilities](/sql/restrictions.md#number-of-conditions) this feature offers, or
operate in a resource constrained environment where running the required database
queries is of concern.

The shadow database which is created by default can be toggled on and off on a per-table basis.
If your data source configuration looked as follows and you wanted to disable the shadow
database creation for the `very_large_table` table, you could include the `maintain_shadow_db`
parameter and give it the value false:

```
{
  "tables": {
    "regular_table": {
      ...
    },

    "very_large_table": {
      ...
      "maintain_shadow_db": false
    },

    ...
  }
}
```

#### Analyst tables

Analyst tables make it possible for analysts to create additional tables in the database via the Aircloak user interface. The main purpose of these tables is to allow analysts to prepare a static snapshot of a potentially long running intermediate query. For example, consider the following query:

```
SELECT col_a, col_b
FROM (
  # possibly slow subquery
  SELECT ...
) subquery
```

If the subquery is taking a long time to complete, running different kinds of queries with variations in the top-level outer query can become very cumbersome. This is where analyst tables can help. They allow analysts to create a snapshot of the data returned by the inner query, and allow querying that snapshot instead.

Analyst tables are created in the air user interface. A table is described via a regular Aircloak `SELECT` query which defines the table structure and its content.

The query must be anonymizing, which means that it must select at least one user id column. Queries which lead to emulation (i.e. which can't be completely offloaded to the database) can't be used to create analyst tables.

When an analyst table is submitted for creation via the user interface, the cloak will create the corresponding table in the database and populate it. The table population is running asynchronously, and depending on the query, it might take a while. The table cannot be used for querying while it is being populated.

Once the table is populated, it can be used as any other table in Aircloak queries. The table can also be used from other analyst tables and views.

It's worth noting that each analyst tables is private, meaning that it can only be used by the analyst who created it.

Analyst tables should conceptually be treated as snapshots. A table won't update if data changes in the source tables. To update the content of the table, an analyst must open it for editing in the air user interface, and then press the "Update" button to trigger the table recreation. The table cannot be used for querying until the recreation has completed.

Since analyst tables can potentially cause additional load on the database server, both in terms of processing and disk-usage, they are by default disabled. To enable this feature, set the "analyst_tables_enabled" property in the data source configuration to `true`.

Currently, analyst tables are only supported on PostgreSQL and Oracle data sources.

If the air name or the datasource name is changed, duplicate copies of analyst tables might appear in the cloak database. This happens because the analyst table name depends on the air name and the datasource name. The database administrator can safely manually delete the obsolete analyst tables should such an event happen.

The administrator can use the table `__ac_analyst_tables_X` (where `X` is an integer) to list analyst tables. This table contains the list of all currently known analyst tables. The administrator can use the following columns to determine which tables are no longer needed:

- `air` - name of the air instance
- `data_source` - name of the data source where the table is created
- `analyst` - the numerical id of the table owner
- `name` - the table name, as seen in the air by its owner
- `db_name` - the name of the table in the database

If the administrator is certain that some analyst tables are no longer needed, for example if an air instance or some datasource have been renamed or decommissioned, they can drop these tables, and delete the corresponding entries from the `__ac_analyst_tables_X` table.

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
"t": {
  "query": "SELECT uid, x, z FROM t",
  "keys": [
    {"user_id": "id"}
  ]
}
```

Alternatively, if the target table has a large number of columns and we don't want to list all of them, we can explicitly
select constants with the hidden columns' names, then star-select everything else. This uses the fact that duplicated
columns are eliminated by only keeping the first instance and then that constants are dropped from the list of exposed
columns. For example:

```
"t": {
  "query": "SELECT 0 AS y, t.* FROM t",
  "keys": [
    {"user_id": "id"}
  ]
}
```

## Running without Docker containers

If you're running the system without Docker containers, there are some additional things that need to be configured.

### Insights Air shadow server

Insights Air requires access to two PostgreSQL database servers. One is used for storing query results, audit logs and user accounts.
This is the PostgreSQL database server described in the [Components of Aircloak Insights](/components.md#components-provided-by-the-customer)-chapter
of these guides. The second PostgreSQL database server normally runs as part of the Insights Air docker container itself.
When docker containers are not used this database server needs to be provided separately. It should be a PostgreSQL database
server of version 9.6. The login credentials provided must be for a superuser or for a user having been given privileges to create and destroy
databases (`CREATEDB`-role) on this database server. They can be configured in the `config.json` configuration file
of the Air component under the `shadow_database` key.

```
...

"shadow_database": {
  "host": string,
  "port": integer,
  "ssl": boolean,
  "user": string,
  "password": string,
  "name": string
},

...
```

Here, the `"name"` parameter configures the name of the database to which the given user can connect. The database name is needed because a PostgreSQL connection can only be established to an existing database. For this purpose, you can use either the `postgres` database, or create a dedicated database. Make sure to grant `CONNECT` permission on the database to the user.

## File permissions

The Aircloak Insights software is run inside a docker container under a user called `deployer`.
The privileges of the software are limited by those of the `deployer` user.
In order for Aircloak Insights to read the configuration files they need [file
permissions](https://en.wikipedia.org/wiki/File_system_permissions#Traditional_Unix_permissions) that allow
everyone to read them.

Unix file permissions distinguish between the rights of the owner of a file, the members of a particular group, and
everyone else. The `deployer` user belongs to the latter of the three, namely the everyone else category.

If you consider file permissions in their symbolic notation (like they are shown when running `ls -la` in the terminal),
then the permissions need to end in `r--`. If you consider the privileges in their numeric notation, then the last digit
needs to be at least a 4 (meaning it grants read privileges).

Below follows a set of file permissions that would work:

```
$ ls -la
-rwxr--r--  25 owner  group     800 Jan  1 00:01 ideal
-rwxrwxr--  25 owner  group     800 Jan  1 00:01 ideal
-rwxr-xr-x  25 owner  group     800 Jan  1 00:01 ok-but-too-permissive
-rwxrw-rw-  25 owner  group     800 Jan  1 00:01 ok-but-too-permissive
-rwxrwxrwx  25 owner  group     800 Jan  1 00:01 ok-but-too-permissive
```

whereas the following set of file permissions _would not work_ because they do not give the `deployer` user permission
to read the file:

```
$ ls -la
-rwxr-----  25 owner  group     800 Jan  1 00:01 missing-read-privileges
-rwxrw----  25 owner  group     800 Jan  1 00:01 missing-read-privileges
-rwxr-x---  25 owner  group     800 Jan  1 00:01 missing-read-privileges
-rwxrwx---  25 owner  group     800 Jan  1 00:01 missing-read-privileges
```

In a unix shell you can add the required read permission with the following command: `chmod o+r file-name`.
The `o` signifies the everyone else category (also known as "other") and the `+r` grants read permission.
