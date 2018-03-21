# Summary

---- Don't alter the line below. Needed as is for automated version number update
## Aircloak Insights - version 18.2.0
---- Don't alter the line above...

- [Introduction](README.md)

## Architectural overview

- [Components of Aircloak Insights](components.md)
  - [Provided by Aircloak](components.md#components-provided-by-aircloak)
  - [Provided by the customer](components.md#components-provided-by-the-customer)
  - [How the components interact](components.md#how-the-components-interact)
- [Deployment](deployment.md)
  - [Overview](deployment.md#overview)
  - [Resource requirements](deployment.md#resource-requirements)
  - [Important notice](deployment.md#important-notice)
- [Scaling](scaling.md)
  - [Low query volume](scaling.md#low-query-volume)
  - [High query volume](scaling.md#high-query-volume)
- [Supported datastores](datastores.md)
  - [PostgreSQL](datastores.md#postgresql)
  - [MySQL and MariaDB](datastores.md#mysql-and-mariadb)
  - [Microsoft SQL Server](datastores.md#microsoft-sql-server)
  - [MongoDB](datastores.md#mongodb)
  - [Feature emulation](datastores.md#emulation-overview)
  - [Notes](datastores.md#database-specific-notes)


## Technical documentation

- [Core language features](sql.md)
  - [Considerations](sql.md#considerations)
- [Restrictions](sql/restrictions.md)
  - [JOINs](sql/restrictions.md#join-restrictions)
  - [Subqueries](sql/restrictions.md#subquery-restrictions)
  - [Top-level HAVING clause](sql/restrictions.md#top-level-having-clause)
  - [Math and function application](sql/restrictions.md#math-and-function-application-restrictions)
  - [Ranges](sql/restrictions.md#ranges)
  - [Range alignment](sql/restrictions.md#range-alignment)
  - [Implicit ranges](sql/restrictions.md#implicit-ranges)
  - [Text operations](sql/restrictions.md#text-operations)
  - [IN, NOT IN, NOT LIKE, and <>](sql/restrictions.md#in-not-in-not-like-and-)
- [Understanding query results](sql/query-results.md)
  - [Pro Tips](sql/query-results.md#pro-tips)
  - [Zero-mean noise](sql/query-results.md#zero-mean-noise)
  - [Low-count filtering](sql/query-results.md#low-count-filtering)
  - [Aggregates](sql/query-results.md#anonymizing-aggregation-functions)
- [Supported functions](sql/functions.md)
  - [Date/time functions](sql/functions.md#datetime-functions)
  - [Working with intervals](sql/functions.md#working-with-intervals)
  - [Mathematical operators](sql/functions.md#mathematical-operators)
  - [Mathematical functions](sql/functions.md#mathematical-functions)
  - [String functions](sql/functions.md#string-functions)
  - [Casting](sql/functions.md#casting)
- [API](api.md)
  - [Authentication](api.md#authentication)
  - [Data sources](api/data_sources.md)
  - [Queries](api/queries.md)
  - [Errors](api/errors.md)
  - [PostgreSQL Message Protocol server](api/psql.md)

## Operations guides
- [Installing the system](ops/installation.md)
  - [Insights Air](ops/installation.md#insights-air)
  - [Insights Cloak](ops/installation.md#insights-cloak)
- [Configuring the system](ops/configuration.md)
  - [Overview](ops/configuration.md#overview)
  - [Insights Air](ops/configuration.md#insights-air-configuration)
  - [Insights Cloak](ops/configuration.md#insights-cloak-configuration)
- [Upgrading](ops/upgrading.md)
- [Monitoring](ops/monitoring.md)
