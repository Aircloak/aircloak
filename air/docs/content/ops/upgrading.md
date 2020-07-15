# Upgrading

We recommend taking the following steps before upgrading to a new version of Aircloak Insights:

1. Check whether there are incompatibilities or config changes needed by reading the upgrade documentation specific
  to the version of Aircloak Insights you are upgrading to. It can be found further down in this document.
2. Take a backup of the Postgres database. This allows you to undo a system upgrade without losing data.

## Postgres backup

Upgrades, with few exceptions, make alterations to the database schema. While some of these are reversible,
others are not. We recommend taking a database backup before upgrading.

You can use [pg_dump](https://www.postgresql.org/docs/12/app-pgdump.html) and [pg_restore](https://www.postgresql.org/docs/current/app-pgrestore.html)
to create and restore a backup. These tools come as part of a standard Postgres installation.

Creating and restoring a backup can be done as follows:

```sh
$ echo "Backing up Insights Air's database"
$ pg_dump -h hostname -U username -p 5432 -d dbname -Fc > backup.sql

$ echo "Restoring Insights Air's database"
$ pg_restore -h hostname -U username -p 5432 --clean -d dbname < backup.sql
```

When issuing these commands you must make sure to replace the parameters (such as `-h` for the hostname and `-U` for the database user)
with ones specific to your particular installation.

# Version 20.2.0

## Insights Air

Due to a bug in earlier versions of Insights Air, it was possible to assign a user managed through Insights Air to a
group managed by LDAP. This would lead to a system state where LDAP sync would no longer work.

Insights Air 20.2 contains a bugfix which prevents such a configuration from being made. Any users that had mistakenly
been assigned to an LDAP group by an administrator will automatically be moved out of the group as part of migration.

For example let's imagine user `alice` is a user managed through Insights Air. She has been assigned to group
`ldap-group` giving her access to query the data source `my-movies`. After upgrading to Aircloak Insights 20.2 she
will have been moved from the `ldap-group` to a new group called `MIGRATED: ldap-group` giving her continued access to
`my-movies`.

# Version 20.1.0

## Insights Air

The `auto_aircloak_export` parameter has been deprecated.
Insights Air will refuse to start if the parameter is present.
Please remove the parameter from your `config.json`-configuration file
before upgrading to this latest version.

### Privacy policy

In versions of Aircloak Insights prior to 20.1 Aircloak would track
pseudonymized usage information for subsequent anonymized analyses.
With version 20.1 this is no longer the case.

The default privacy policy has been updated to reflect this change.
You might want to alter the privacy policy of your installation to
reflect these changes as well.

The updated and simplified language of the privacy policy can be found
here: [20.1.0 privacy policy](upgrade/2001_privacy_policy.md).

# Version 19.2.0

## Insights Cloak

### Tables configuration

__The old style of configuring tables will stop working in version 19.3.__

The table fields `user_id`, `projection` and `decoders` have been marked as deprecated. A warning will be issued for
each usage of these fields in a datasource configuration file. Configured tables should use the new `keys` field and/or
the `query` field. Refer to the [Insights Cloak configuration](/ops/configuration.html#insights-cloak-configuration)
for more details.

#### Replacing the `user_id` field

Setting the user id for a table is now done by setting a table key with the type `"user_id"`.

Old style table configuration:
```json
"tables": {
  "accounts": {
    "user_id": "client_id",
    ...
  }
}
```
New style table configuration:
```json
"tables": {
  "accounts": {
    "keys": [{"user_id": "client_id"}],
    ...
  }
}
```

In order to directly expose a table that doesn't contain personal data, the `content_type` field has to be set to
`non-personal`.

Old style table configuration:
```json
"tables": {
  "products": {
    "user_id": null,
    ...
  }
}
```
New style table configuration:
```json
"tables": {
  "products": {
    "content_type": "non-personal",
    ...
  }
}
```

#### Replacing the `projection` field

Tables that do not contain a user id column will need to have configured keys through which they can be joined to other
tables that contain the required user id field. This mechanism allows for more explicit handling of database table by
the analyst.

Old style table configuration:
```json
"tables": {
  "accounts": {
    "user_id": "customer_id"
  },
  "transactions": {
    "projection": {
      "table": "accounts",
      "foreign_key": "account_id",
      "primary_key": "id"
    }
  }
}
```
New style table configuration:
```json
"tables": {
  "accounts": {
    "keys": [
      {"user_id": "customer_id"},
      {"account": "id"}
    ]
  },
  "transactions": {
    "keys": [
      {"account": "account_id"}
    ]
  }
}
```

#### Replacing the `decoders` field

Data can be pre-processed by creating a virtual table, which configures a table from a query, similar to an SQL view.

Old style table configuration:
```json
"tables": {
  "transactions": {
    "decoders": [
      {"method": "text_to_datetime", "columns": ["beginDT", "endDT"]},
      {"method": "text_to_real", "columns": ["price"]}
    ]
    ...
  }
}
```
New style table configuration:
```json
"tables": {
  "transactions": {
    "query": "SELECT CAST(beginDT AS datetime), CAST(endDT AS datetime), CAST(price AS real), * FROM transactions",
    ...
  }
}
```

## Insights Air

### Configuration

There is a new and required `name` parameter in the configuration file, used to uniquely identify the Insights Air
instance in the system. This property will need to be added to existing configuration files in order for them to
load properly.

# Version 18.5.0

## Insights Air

### Change in monitoring report format

The format of the reported memory stats in the monitoring API endpoint has changed.
Please consult the [monitoring](/ops/monitoring.md) guide for information on the new
format.

# Version 18.4.0

## Insights Air

### Altered privacy policy flow

The privacy policy flow has been changed. Where new users were previously required to accept the privacy policy
before they could start using the system, they now only have the privacy policy available for perusal at their
own pleasure. From version 18.4.0 it is the responsibility of the organisation hosting the Aircloak Insights
installation to inform their analysts about the privacy policy and their rights.

The default content has also been updated to reflect these changes. You can see the updated default content here:
[Privacy policy default content](upgrade/1804_privacy_policy.md).

### Non-docker deployments

If you are running Aircloak Insights without the use of docker containers, you will now have to provide a
second PostgreSQL database server instance. More details can be found in the [Running without Docker containers](/ops/configuration.html#running-without-docker-containers)
section of the operations guides.

# Version 18.3.0

## Insights Cloak

### Static analysis of columns

From version `18.3.0` onwards, Insights Cloak performs an analysis of the columns in a data source when booting.
The analysis determines which columns are likely to isolate users.
Such columns have mostly user-specific values, and therefore behave much like the user-id column. Consider columns containing email addresses or social security numbers as examples.
Columns that are deemed to be isolating get a set of extra restrictions applied to them.
For more information, please consult the [Isolating columns](/sql/restrictions.md#isolating-columns)
section of the restrictions chapter.

Depending on the database size the static analysis might take quite some time to complete. The Insight Cloak
supports caching the results to avoid having to reperform the analysis when Insights Cloak is restarted or upgraded.
To enable caching you have to mount a folder into the Insights Cloak container under the path `/persist`.
Your `docker run ...` command would have to be updated to look something like this:

```
docker run ... \
  -v cloak_persist_folder:/persist \
  ...
```

Where `cloak_persist_folder` is the path you want the cache to be stored at on your host system.
Depending on your setup it might be something like `/aircloak/cloak/cache`.

__The Cloak container needs both read and write permissions to this folder.__

If the static analysis puts undue stress on your data source, or does not complete within a reasonable time, please
consider manually classifyng your columns. More information on how this is done can be found
[here](configuration.md#insights-cloak-configuration) under the heading _Manually classifying isolating columns_.
