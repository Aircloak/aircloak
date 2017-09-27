# Summary

---- Don't alter the line below. Needed as is for automated version number update
## Aircloak Insights - version 17.4.0
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

## Technical documentation

- [Core language features](sql.md)
  - [Considerations](sql.md#notes)
  - [Restrictions](sql/restrictions.md)
    - [JOINs](sql/restrictions.md#join-restrictions)
    - [Subqueries](sql/restrictions.md#subquery-restrictions)
    - [Math and function application](sql/restrictions.md#math-and-function-application-restrictions)
    - [Inqualities](sql/restrictions.md#inequality-restrictions)
- [Understanding query results](sql/query-results.md)
  - [Probing](sql/query-results.md#probing)
  - [Low-count filtering](sql/query-results.md#low-count-filtering)
  - [Added noise](sql/query-results.md#adding-noise)
  - [Anonymization functions](sql/query-results.md#anonymization-functions)
- [Supported functions](sql/functions.md)
  - [Date/time functions](sql/functions.md#datetime-functions)
  - [Mathematical operators](sql/functions.md#mathematical-operators)
  - [Mathematical functions](sql/functions.md#mathematical-functions)
  - [String functions](sql/functions.md#string-functions)
  - [Casting](sql/functions.md#casting)
- [API](api.md)
  - [Authentication](api.md#authentication)
  - [Data sources](api/data_sources.md)
  - [Queries](api/queries.md)
  - [Errors](api/errors.md)
  - [Postgres Message Protocol server](api/psql.md)

## Operations guides
- [Installing the system](installation.md)
  - [Insights Air](installation.md#insights-air)
  - [Insights Cloak](installation.md#insights-cloak)
- [Configuring the system](configuration.md)
  - [Overview](configuration.md#overview)
  - [Insights Air](configuration.md#insights-air-configuration)
  - [Insights Cloak](configuration.md#insights-cloak-configuration)
- [Upgrading](upgrading.md)
- [Monitoring](monitoring.md)
