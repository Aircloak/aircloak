# Datastores

Aircloak Insights supports a range of datastores through its Insights Datasource Connectors.
The query interface presented to the analysts is uniform across all datastore types.

For details on feature emulation and the performance impact of emulation,
please consult the [feature emulation](#emulation-overview) overview.

Aircloak Insights ships with Insights Datasource Connectors for the following datastores:

- Microsoft SQL Server, versions 2012 R2 and newer
- MongoDB, versions 3.4 and newer
- MySQL, version 5 and newer, and MariaDB, version 10.1 and newer
- PostgreSQL, version 9.1 and newer
- Oracle 12c
- Cloudera Impala, version 2.10 (CDH 5.13.x) and newer

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

```SQL
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

```SQL
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

### Emulated functions

This section lists the functions which will cause a query to be emulated.

#### Microsoft SQL Server

  - `btrim`
  - `date_trunc`
  - `trim`

#### MongoDB

- `btrim`
- `cast` - in some cases
- `date_trunc`
- `hex`
- `ltrim`
- `rtrim`
- `trim`

__Notes__

The following constructs are not natively supported on this data source and will require emulation:

* Using an `ASC` order with `NULLS LAST` or a `DESC` order with `NULLS FIRST`
* Global aggregators (aggregators without grouping) in standard queries

#### MySQL and MariaDB

  - `btrim`
  - `date_trunc`
  - `trim`

#### Cloudera Impala

- `ltrim` when a custom string is specified as its second argument.
- `rtrim` when a custom string is specified as its second argument.


## Database-specific notes

This section provides additional notes specific for each supported database.

### MongoDB

#### Schema detection

Collections in MongoDB do not have a fixed schema, whereas schemas are required by Aircloak Insights. In order to
establish a schema that can be used, Aircloak Insights will traverse the collections of a database upon boot. This
produces a best effort estimate of the available fields and their data types.

#### Mapping from documents to tables

##### Nested documents

Aircloak Insights flattens nested documents.
Fields that in MongoDB are part of a sub-document are in Aircloak Insights
given hierarchical names instead.

For example, the following document:

```json
{
  "person": {
    "sibling": {
      "name": <...>
    }
  }
}
```

would result in a column named: `person.sibling.name`.


##### Arrays

Aircloak Insights creates an additional table per array contained within a document.
These tables contain the columns of the parent table, as well as those of the objects
contained within the array.

For example, the following document in a collection called `users`:

```json
{
  "name": <...>,
  "siblings": [
    {
      "name": <...>
    }
  ]
}
```

would result in a table called `users` with the single column `name`, as well as the additional
table `users_siblings` containing a `siblings.name` in addition to the `name` column.


#### JOINs

MongoDB only supports `INNER JOIN` natively. Aircloak Insights will emulate all other JOIN-types. Furthermore, when a
collection is sharded, even `INNER JOIN` has to be emulated.

### Oracle

#### Safe functions {#oracle-safe-functions}

By default, mathematical operations in Oracle are unsafe (see [the section on Column
bounds](/sql/restrictions.md#column-bounds) for more on this topic). Aircloak provides safe versions of these
operations, but they require manual installation by the database server administrator. The benefits of doing so are
increased query performance when mathematical operations are involved. In order to install (or update) the Aircloak
user defined functions (UDFs), perform the following steps:

1. Run the `dbmsstdx.sql` script provided by Oracle to enable user-defined functions. You only need to do this once.
  * You can use `find $ORACLE_HOME -name dbmsstdx.sql` in the command line to find the script. If you cannot locate the
  script in your installation, contact Oracle support for an appropriate script to use with your version.
  * Run `$ sqlplus / as sysdba` when logged in as the linux user who owns the oracle installation to login as the
  `sysdba` role.
  * Run `@/path/to/dbmsstdx.sql` in the SQL prompt that appears.
2. Run the contents of <a href="/aircloak_udfs.sql" target="_blank">aircloak_udfs.sql</a> on the schema for
   which you want to enable the Aircloak UDFs. This step will also update the UDFs if a newer version is provided by
   Aircloak.
  * Log in as `sysdba` as in the previous point.
  * Run `ALTER SESSION SET CURRENT_SCHEMA = schema_name` where `schema_name` is the schema for which you want to enable
  the UDFs.
  * Run `@/path/to/aircloak_udfs.sql`.
3. Update the configuration for your Oracle data source.
  * In the `parameters` section add an `aircloak_udfs` key with a `true` value:
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

-  Data source user must have the `USAGE` privilege.
