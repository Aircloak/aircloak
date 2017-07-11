# Datastores

Aircloak Insights supports a range of datastores through its Insights Datasource Connectors.
The query interface presented to the analysts is uniform across all datastore types.

For details on feature emulation and the performance impact of emulation,
please consult the [feature emulation](#emulation-overview) overview.

Aircloak Insights ships with Insights Datasource Connectors for the following datastores:

- [Microsoft SQL Server](#microsoft-sql-server)
- [MySQL and MariaDB](#mysql-and-mariadb)
- [Postgres](#postgres)
- [MongoDB](#mongodb)

If your preferred datastore is not in the list, please contact Aircloak.

## Microsoft SQL Server

The following versions of Microsoft SQL Server are supported:

- SQL Server 2016
- SQL Server 2014
- SQL Server 2012 R2

## MySQL and MariaDB

MySQL version 5 and more recent is supported.
We also support the MySQL fork MariaDB from version 10.1 and onwards.

`FULL OUTER JOIN`'s are not natively supported by MySQL and MariaDB.
Aircloak Insights replaces any insatnce of a `FULL OUTER JOIN` with a
combination of a `LEFT OUTER JOIN` and a `RIGHT OUTER JOIN`.

## Postgres

Postgres 9.1 and more recent of Postgres is supported.

## MongoDB

Support for versions `3.2` and more recent.

### Schema detection

Collections in MongoDB do not have a fixed schema, whereas schema's are required by Aircloak Insights. In order to establish a schema that can be used Aircloak Insights
will traverse the collections of a database upon boot. This produces a best effort
estimate of the available fields and their data types.

### Mapping from documents to tables

#### Nested documents

Aircloak Insights flattens nested documents into a flat structure.
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


#### Arrays

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


### JOINs

MongoDB versions more recent than 3.2 support `INNER JOIN`'s. Aircloak Insights
will emulate all unsupported JOIN-types. Furthermore, when a collection is sharded,
even `INNER JOIN`'s on recent versions of MongoDB have to be emulated.


## Emulation overview

The level of features natively supported by the different datastores differ. All the same,
the query interface provided to an analyst remains the same. This is achieved by Aircloak Insights
emulating functionality that is not natively supported by the underlying datastore.

While convenient, this does incurr performance overhead as all the data needed for the analysis
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
    --> a3 (executed in the datasource)
      --> a4 (executed in the datasource)
        --> t2 (executed in the datasource)
```

### What triggers emulation

The following section describes which functions cause a query to become emulated, and which do not.


|               | MS SQL   | MySQL    | Postgres | MongoDB >= 3.2 | MongoDB < 3.2 |
|--------------:|:--------:|:--------:|:--------:|:--------------:|:-------------:|
| %             | ✔        | ✔        | ✔        | ✔              | ✔             |
| &#124;&#124;  | ✔        | ✔        | ✔        | ✔              | ✔             |
| *             | ✔        | ✔        | ✔        | ✔              | ✔             |
| +             | ✔        | ✔        | ✔        | ✔              | ✔             |
| -             | ✔        | ✔        | ✔        | ✔              | ✔             |
| /             | ✔        | ✔        | ✔        | ✔              | ✔             |
| ^             | ✔        | ✔        | ✔        | ✔              | ✔             |
| abs           | ✔        | ✔        | ✔        | ✔              | Emulated      |
| avg           | ✔        | ✔        | ✔        | ✔              | ✔             |
| btrim         | Emulated | Emulated | ✔        | Emulated       | Emulated      |
| ceil          | ✔        | ✔        | ✔        | ✔              | Emulated      |
| ceiling       | ✔        | ✔        | ✔        | ✔              | Emulated      |
| concat        | ✔        | ✔        | ✔        | ✔              | ✔             |
| count         | ✔        | ✔        | ✔        | ✔              | ✔             |
| date_trunc    | Emulated | Emulated | ✔        | Emulated       | Emulated      |
| day           | ✔        | ✔        | ✔        | ✔              | ✔             |
| div           | ✔        | ✔        | ✔        | ✔              | ✔             |
| floor         | ✔        | ✔        | ✔        | ✔              | Emulated      |
| hour          | ✔        | ✔        | ✔        | ✔              | ✔             |
| lcase         | ✔        | ✔        | ✔        | ✔              | ✔             |
| left          | ✔        | ✔        | ✔        | ✔              | ✔             |
| length        | ✔        | ✔        | ✔        | ✔              | ✔             |
| lower         | ✔        | ✔        | ✔        | ✔              | ✔             |
| ltrim         | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| max           | ✔        | ✔        | ✔        | ✔              | ✔             |
| median        | Emulated | Emulated | ✔        | Emulated       | Emulated      |
| min           | ✔        | ✔        | ✔        | ✔              | ✔             |
| minute        | ✔        | ✔        | ✔        | ✔              | ✔             |
| mod           | ✔        | ✔        | ✔        | ✔              | ✔             |
| month         | ✔        | ✔        | ✔        | ✔              | ✔             |
| pow           | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| quarter       | Emulated | ✔        | ✔        | ✔              | Emulated      |
| right         | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| round         | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| rtrim         | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| second        | ✔        | ✔        | ✔        | ✔              | ✔             |
| sqrt          | ✔        | ✔        | ✔        | ✔              | Emulated      |
| stddev        | ✔        | ✔        | ✔        | Emulated       | Emulated      |
| substring     | ✔        | ✔        | ✔        | ✔              | ✔             |
| sum           | ✔        | ✔        | ✔        | ✔              | ✔             |
| trunc         | ✔        | ✔        | ✔        | ✔              | Emulated      |
| ucase         | ✔        | ✔        | ✔        | ✔              | ✔             |
| upper         | ✔        | ✔        | ✔        | ✔              | ✔             |
| weekday       | ✔        | ✔        | ✔        | ✔              | ✔             |
| year          | ✔        | ✔        | ✔        | ✔              | ✔             |
