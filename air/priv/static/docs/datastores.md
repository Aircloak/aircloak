# Datastores

Aircloak Insights supports a range of datastores through its Insights Datasource Connectors.
The query interface presented to the analysts is uniform across all datastore types.

For details on feature emulation and the performance impact of emulation,
please consult the [feature emulation](#emulation-overview) overview.

Aircloak Insights ships with Insights Datasource Connectors for the following datastores:

- PostgreSQL, version 9.6 and newer
- Oracle 12c
- Apache Impala, version 2.10 and newer, as included in Cloudera's Apache Hadoop Distribution (CDH) version 5.13.X and newer

If your preferred datastore is not in the list, please contact Aircloak.

## Emulation overview

The level of features natively supported by the different datastores differ. Despite this
the query interface remains the same irrespective of the datastore used.
This is achieved by Aircloak Insights
emulating functionality that is not natively supported by the underlying datastore.

While convenient, this does incur performance overhead as all the data needed for the analysis
is loaded into Insights Cloak, rather than being processed in the datastore itself.

### What gets emulated

Insights Cloak will attempt to perform as much of the processing in the datastore itself as possible.
Concretely that means that sub-queries of an emulated query that do not themselves need emulation will be run in the datastore.
If one half of a JOIN requires emulation then the JOIN itself will take place inside Insights Cloak.
The other branch, unless it also needs emulation, will be evaluated inside the datastore.

In the following examples, we use an abstract query syntax such that a query like:

```sql
SELECT a, b, c
FROM (
  SELECT e, f, g
  FROM t1
) a1
```

is shown as:

```
query
  --> a1
    --> t1
```

and

```sql
SELECT a, b, c
FROM (
  SELECT e, f, g
  FROM t1
) a1 INNER JOIN t2
```

is shown as:

```
query
  --> a1
    --> t1
  --> t2
```

Given this syntax, the following query:

```
query
  --> a1
    --> a2 (needing emulation)
      --> a3
        --> t1
```

will be emulated as follows:

```
query (emulated)
  --> a1 (emulated)
    --> a2 (emulated)
      --> a3 (executed in the datasource)
        --> t1 (executed in the datasource)
```

A JOIN query like the following:

```
query
  --> a1
    --> a2 (needing emulation)
      --> t1
    --> a3
      --> a4
        --> t2
```

will be emulated as follows:

```
query
  --> a1 (emulated)
    --> a2 (emulated)
      --> t1 (executed in the datasource)
    --> a3 (emulated)
      --> a4 (executed in the datasource)
        --> t2 (executed in the datasource)
```

You can use the [EXPLAIN statement](/sql.md#describing-the-query-plan) to see which components of a query will be emulated.

## Emulated functions

This section lists the functions which will cause a query to be emulated.

### Apache Impala

- `ltrim` when a custom string is specified as its second argument.
- `rtrim` when a custom string is specified as its second argument.

**Notes**

Selecting or filtering on intervals causes the query to be emulated.
The emulation happens because Apache Impala has no native data type for intervals.

## Database-specific notes

This section provides additional notes specific for each supported database.

### Oracle

#### Safe functions :id=oracle-safe-functions

By default, mathematical operations in Oracle are unsafe (see [the section on Column
bounds](/sql/restrictions.md#column-bounds) for more on this topic). Aircloak provides safe versions of these
operations, but they require manual installation by the database server administrator. The benefits of doing so are
increased query performance when mathematical operations are involved. In order to install (or update) the Aircloak
user defined functions (UDFs), perform the following steps:

1. Run the `dbmsstdx.sql` script provided by Oracle to enable user-defined functions. You only need to do this once.

   - You can use `find $ORACLE_HOME -name dbmsstdx.sql` in the command line to find the script. If you cannot locate the
     script in your installation, contact Oracle support for an appropriate script to use with your version.
   - Run `$ sqlplus / as sysdba` when logged in as the linux user who owns the oracle installation to login as the
     `sysdba` role.
   - Run `@/path/to/dbmsstdx.sql` in the SQL prompt that appears.

2. Run the contents of <a href="/docs/aircloak_udfs.sql" target="_blank">aircloak_udfs.sql</a> on the schema for
   which you want to enable the Aircloak UDFs. This step will also update the UDFs if a newer version is provided by
   Aircloak.

   - Log in as `sysdba` as in the previous point.
   - Run `ALTER SESSION SET CURRENT_SCHEMA = schema_name` where `schema_name` is the schema for which you want to enable
     the UDFs.
   - Run `@/path/to/aircloak_udfs.sql`.

3. Update the configuration for your Oracle data source.

   - In the `parameters` section add an `aircloak_udfs` key with a `true` value:

```json
  {
    ...
    "parameters": {
      ...
      "aircloak_udfs": true
    }
  }
```

### PostgreSQL

- Data source user must have the `USAGE` privilege.

### Apache Impala

- Complex types (structs and arrays) are not supported.
