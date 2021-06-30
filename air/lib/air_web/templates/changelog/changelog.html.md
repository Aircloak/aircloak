## Version 21.2.0 - Long term support

Version 21.2.0 marks the first Aircloak Insights long term support release.
It is called Aircloak Insights Evergreen.

### **Breaking changes**

- Support removed for MariaDB, MySQL and Microsoft SQL Server.

### Bugfixes

- The audit log and the query performance page would not load correctly on installations configured to use long polling rather
  than web sockets.
- Show the "Not Found" page on invalid paths.

### New features

- The system status page now shows statistics on failed and successful login attempts, currently active user sessions,
  as well as historic query execution statistics.
- Container logs are stored in the Postgres database, and shown on the system status page. Old log entries are removed after 15 days.

### Changes

- The list of actively running queries has been moved from the activity monitor to the queries section of the admin interface.
- The activity monitor page has been renamed to system status and now incorporates the warnings page.

## Version 21.1.1

### Enhancements

- Made execution time of queries more consistent to prevent some types of timing attacks.
- Debug logs for queries always show the `ingesting data` state.
  This prevents an attack where the presence of this line could be used to determine whether a query result contains any rows.

### Changes

- It is no longer possible to observe the difference between the database
  processing phase and the data ingestion phase of a query.

## Version 21.1.0

### **Breaking changes**

- The Oracle column type `date` now maps to an Aircloak `datetime`.
  This accounts for the fact that `date`s in Oracle can also include a time component.
  Existing analyst tables which include `date` columns must be updated to reflect this change.
  Analyst tables are updated by resaving their definition in Insights Air interface.

### New features

- Added support for `UNION` between non-restricted queries.
- Analysts now get feedback about errors and query structure as they type.
- Administrators now get a dashboard for exploring query performance issues.

### Enhancements

- Simpler offloaded queries are generated when using bounds analysis.
- Virtual table queries don't use safe operators anymore, leading to better performance in some cases.
- Indicate query starting location in per-query compilation error messages.

### Changes

- Data source query history is now loaded by default on page load.

## Version 20.3.0

### **Breaking changes**

- The `%` operator is now considered an implicit range.
- The `enable_case_support` configuration setting has been removed.
- Constants in restricted `IN` clauses have to be from the list of frequent values, unless the
  `allow_any_value_in_in_clauses` setting is enabled in the Insights Cloak configuration file.
- Only a single `COUNT`, `MIN` or `MAX` aggregator is now allowed in restricted `IN` and `<>` conditions.

### New features

- Added support for `EXPLAIN` query. The result will describe the types of queries and subqueries.
- Administrators can now manually reload a cloak's data sources.

### Enhancements

- `month`, `quarter`, `year` and `extract(month/quarter/year)` are no longer considered implicit ranges.
- The audit log has been redesigned. It now shows significantly more data by default.
- Installations using the [Diffix Explorer integration](https://hub.docker.com/r/diffix/explorer) now have
  support for showing sample tables in the tables list on the query page.

### Bugfixes

- Cached connections are now forcefully terminated when a data source is detected as broken.
- Ignore trailing semicolon in analyst tables definitions.
- Fixed offloading of date/interval operations on MySQL.
- Fixed crash on invalid UTF8 strings when using ODBC-based drivers.
- Fixed outdated examples of restricted queries in the docs.
- Searching the documentation could result in the browser window crashing.

### Changes

- `CASE` support is now enabled by default. `WHEN` clause constants have to be from the list of frequent values,
  unless the `allow_any_value_in_when_clauses` setting is enabled in the Insights Cloak configuration file.

### Deprecation

- With the introduction of an improved online docs experience we deprecated offline docs (PDF, epub, and mobi).

## Version 20.2.1

### Bugfixes

- Fixed crash when loading data source tables in the sidebar.

### Changes

- Automatic comment loading can be disabled for data sources by setting the `load_comments` flag to `false`.

## Version 20.2.0

### **Breaking changes**

- Default value for `max_rare_negative_conditions` is now 0.
- The `IS [NOT] NULL` operator is restricted to clear expressions.
- Aggregators are restricted to clear expressions.
- Dropped support for non-datetime/interval arithmetic.
- `floor`, `ceil` and `cast(real as integer)` are now considered implicit ranges.
- Removed support for MongoDB backends

### New features

- Added an integration for [Diffix Explorer](https://github.com/diffix/explorer). [If enabled](/docs/#/ops/installation?id=diffix-explorer), analysts will be given an anonymized overview of the contents of columns in your data sources.
- Comments can be added to tables and columns. Database comments are automatically loaded from configured tables.

### Enhancements

- Redesigned user interface with a focus on consistency and easier navigation.
- Added overflow protection for date arithmetic. Oracle UDFs have to be reloaded.
- Improved performance for join-timing protection.
- Allow tighter date ranges by limiting maximum alignment to quarters.
- Improve range alignment algorithm to produce tighter bounds.
- Ranges that end at the maximum value of a type now include the maximum value.
- Allow month-aligned constant date between columns expressions.
- Administrators can now convert analyst tables into views.
- Various query interface improvements such as supporting deleting queries and showing when they were run.
- Allow ranges with mixed date and datetime boundaries.
- Improved range alignment messages.
- The rate at which column analysis queries are executed can be specified in configuration.
- Analyst tables will utilize select hints in Oracle data sources.

### Bugfixes

- Fixed filtering of censored values in standard queries.
- Verify implicit range usage on both sides of a condition.
- Fixed interaction between the column analysis cache and aliased tables.
- Native users could accidentally be assigned to LDAP groups which in turn would break the LDAP sync.
  As part of upgrading to Aircloak Insights 20.2 all users managed through Insights Air that have been
  assigned to an LDAP group will be migrated out of the group and into a transitionary group. This ensures
  all users retain access to all the data sources they expect, but will lead to additional groups being
  created.
- Fixed crash when executing query over analyst table containing filters over the selected columns.

### Changes

- Increased the minimum threshold for non-count stats-based aggregators.
- To celebrate Diffix Dogwood, the most recent version of our anonymization algorithm Diffix, the login screen now shows images of Dogwood trees

## Version 20.1.4

### Bugfixes

- Fixed invalid optimization when executing queries over analyst tables.

## Version 20.1.3

### Bugfixes

- Fixed duplication of noise layers when using `count(distinct column)` aggregators.
- Fixed incorrect rounding in Oracle when using `bucket` functions.

## Version 20.1.2

### Bugfixes

- Fixed crash when executing query over analyst table.
- Fixed crash when using ranges with different boundary types.

## Version 20.1.1

### New features

- Support for excluding columns from a data source table. This can be done using the `exclude_columns` parameter.
- Support for marking columns as unselectable. This can be done using the `unselectable_columns` parameter.

### Bugfixes

- Fixed handling of dotted table names and aliases.
- Fixed performance degradation bug introduced in version 20.1.0.
- Fixed periodically occurring bug that would prevent queries from being run.
- Fixed high CPU usage after startup caused by shadow values cache initialization.

## Version 20.1.0

### **Breaking changes**

- Support for `median` was removed.
- Support for `distinct` modifier was removed for all aggregate functions except `count` and `count_noise`.
- Support for `SAMPLE_USERS` was removed.
- Support for the `hash` function was removed.
- Minimum Oracle version supported is now 12c.
- The minimum supported version of Postgres is now 9.6 (dropping support for version 9.1 through 9.5).
- Support for the `auto_aircloak_export` configuration parameter in the Insights Air config was removed.
  Consult the [Upgrade guide](/docs/#/ops/upgrading) for additional information.
- Support for some obsolete data source configuration features was removed: decoders, projections, explicit
  user_id-field.
- Anonymizing queries using raw user_id columns are rejected instead of automatically censoring the user_id column.

### New features

- Added beta support for Apache Impala (Cloudera Distribution) data sources.

### Enhancements

- When a user changes their password all their other sessions are automatically revoked.
- Admins can add custom messages to the login and main screens.
- Admins can add performance hints to Oracle queries from the cloak's data source config file, by setting the
  `select_hints` field in the `parameters` section.
- The Oracle Instant Client version 18.3 is bundled with the container and no longer needs to be
  provided separately.
- Various data source connection timeouts can now be adjusted in the Cloak config file, under the `timeouts` field.
- Improved support for boolean expressions.
- Allowed inequalities between datetime columns and the current date.
- Added support for `CASE` statements in [standard queries](/docs/#/sql?id=query-and-subquery-types).
  Experimental support for [restricted queries](/docs/#/sql?id=query-and-subquery-types) can be enabled
  in the Cloak config using the `enable_case_support` flag.
- The HTTP REST API query result endpoint no longer returns internal logging data.
- The number of analysis queries needed when multiple copies of a data source exist was reduced.
- The query editor and query results interface was made wider and more suitable for larger screens.

### Bugfixes

- Fixed detection of recursive aggregators usage inside the `HAVING` clause.
- Various fixes for Oracle data source:
  - the parameter order of the `trim` function in the generated SQL was fixed
  - date/time conversion was not always correct
- Views and analyst tables now appear in popular analytics tools such as Tableau.
- Fixed verification of isolated columns usage in non-clear expressions in the `SELECT` clause.

### Changes

- The set of query restrictions was simplified and clarified.
- Allow date ranges from `1900-01-01` to `9999-12-31`.
- Date function `weekday` is now consistent across data sources.
  Returned values are in interval 1 (Sunday) to 7 (Saturday).
  This behavior may change if database defaults are modified.
- Aircloak Insights no longer tracks pseudonymized usage information. The default
  privacy policy has been simplified to reflect this change. Please consult
  the [Upgrade guide](/docs/#/ops/upgrading) for further information.

## Version 19.3.0

### **Breaking changes**

- `GROUP BY` clause is no longer allowed to contain constants.
- Minimum supported MongoDB version is now `3.6`.

### Enhancements

- The LDAP section has a `Sync now` button in `Admin -> Users` and `Admin -> Groups`
- Improved offloading of joins on MongoDB.
- It is possible to force long polling instead of websocket as a transport mechanism for pushing notifications to browsers. See [Insights Air configuration](/docs/#/ops/configuration?id=insights-air-configuration) for details.
- Enabled statistics-based min/max aggregators (with a higher users count threshold).
- Enabled statistics-based count(distinct) aggregators.
- Added protection against join timing attacks.
- Added protection against exceptions in offloaded functions attacks.
- Added support for `GROUPING SETS`, `ROLLUP` and `CUBE`
- Reduced restrictions for columns-only conditions.

### Bugfixes

- Fixed handling of `null` and `undefined` values in MongoDB data sets.
- Fixed parsing of data source configuration files using the `keys` and `content_type` fields simultaneously.
- Anonymization restrictions were incorrectly applied to top-level `HAVING`-clauses

### Changes

- Support for Apache Drill was deprecated.

### Known issues

- Views and analyst tables are not tracked in the shadow DB, so they won't appear in \d commands issued from psql or
  in some tools that use pg-specific means of detecting the schema.

## Version 19.2.0

[Upgrade guide](/docs/#/ops/upgrading)

### **Breaking changes**

- The air configuration must contain the mandatory `name` property in the Insights Air configuration.
  See [Insights Air configuration](/docs/#/ops/configuration?id=insights-air-configuration) for details.
- When quoting `db_name` in drill data sources, the double quote (") character must be used instead of backtick (\`).

### Bugfixes

- Fixed date/time literals support on Oracle.
- Fixed crash in anonymising `median` and `stddev` aggregators.

### Features

- Support noise aggregators in non-anonymising queries.
- Support for current date/time functions.
- Support for signing out all sessions of a user from the settings page or from the admin panel
- Support for analyst tables. See the [Analyst tables](/docs/#/ops/configuration?id=analyst-tables) section for details.

### Enhancements

- Added support for configuring the maximum allowed number of concurrent PostgreSQL connections accepted by the Insights Air. See the section on configuring [Insights Air PostgreSQL interface](/docs/#/ops/configuration?id=insights-air-postgresql-interface-configuration) for details.
- Added the support for the `VARIANCE` function.
- Added validation of virtual queries.
- Implemented offloading of complex per-user grouping.
- Inequalities between raw columns are now allowed both as `col1 >/>=/</<= col2` and `col1 BETWEEN col2 AND col3`
- Extended support for filters in the `HAVING` clause.
- Resetting the password automatically signs out all sessions

### Changes

- Removed support for `extract_words`.
- Password reset tokens and links will now expire after a single use. All password reset linkes/tokens generated with a
  previous version will be invalid as a consequence of this change.

## Version 19.1.0

### Features

- The share button under a query result allows creating permalinks to queries
- You can statically configure Aircloak Insights for fully automated deployments
- Support for limiting maximum number of simultaneous queries per each cloak. See the section on configuring [Insights Cloak](/docs/#/ops/configuration?id=insights-cloak-configuration) for details.
- Faster statistics-based (no-uid) anonymization implemented for certain cases.
- Support for Oracle 11g

### Enhancements

- Per-user data aggregation is offloaded to the database in some cases.

### Changes

- The format in which cloak memory is reported has changed
- By default, the cloak accepts at most 10 simultaneous queries. This setting can be changed with the `max_parallel_queries` cloak configuration parameter. See the section on configuring [Insights Cloak](/docs/#/ops/configuration?id=insights-cloak-configuration) for details.

## Version 18.4.0

### Features

- Support for LDAP for authentication and authorization - also enables using any
  directory service that exposes an LDAP interface, like Active Directory.
- Support for Drill data sources making it possible to query MapR-DB, HBase, and Hive,
  as well as log, JSON and parquet files stored directly on disk, HDFS, or AWS S3.

### Bugfixes

- Correctly typecheck parameters given to `IN`-style `WHERE`-clauses.

### Enhancements

- Support for transferring binary data when using the RODBC version of data source drivers.
- Performance improvements in the failed queries view for systems with high numbers of queries.
- Improved support for external dashboards and tools.
- All Aircloak SQL functions are properly highlighted in the Insights Air SQL-editor.

### Changes

- Noise parameters changed. Query results will differ from what they were in the past.
- The Aircloak native RODBC connector is now used by default over the previous ODBC driver.
- The depth of partial column censoring is now limited to 3 by default. This parameter can be changed in the config.

## Version 18.3.1

### Bugfix

- Fix for queries being improperly rejected when a condition included two isolating columns.

## Version 18.3.0

### **Breaking change**

- ODBC drivers for SAP HANA must be provided by the client and mounted.
  See the [Installation guide](/docs/#/ops/installation) for more info.

### Features

- Added support for userless tables and non-anonymising queries.
- Users passwords can now be reset using a link provided by the system.
  administrator or via the commandline interface.
- Support for the SAP IQ data store.

### Bugfixes

- Fix for queries being rejected when one side of a range is negative.
- Fix redundant scanning of projected tables during cloak initialization.
- HTTP REST API for query results returned JSON with some duplicate keys.
- Fix execution of `ORDER BY`-clause in combination with a `GROUP BY`-clause in queries on MongoDB.

### Enhancements

- Improve identification of which conditions need emulation.
- Support for date and time typed literals.
- `SAMPLE_USERS` no longer causes SAP Hana queries to get emulated.
- The cloak classifies columns by whether they isolate users or not, further improving the anonymisation as a result.
- Improve load performance on the activity monitor page for installations with high numbers of queries.

### Changes

- Improve the consistency between the different ways of computing noisy users counts; some results may change.

## Version 18.2.3

### Bugfixes

- Propagates `sample_rate` config value when loading the database tables needed for virtual tables.

## Version 18.2.2

### Bugfixes

- Fix crash when a postgres connection times out after retrieving all needed data.

## Version 18.2.1

### Features

- Support for specifying a privacy policy.
- An analyst can download all the information kept about them.
- Support for fully deleting all data recorded about an analyst's usage of the system.

## Version 18.2.0

### Features

- Support for virtual tables.

### Enhancements

- In many cases more data is now returned as a result of selectively merging low count buckets by censoring
  columns individually from right to left.
- Query execution times can be reduced by parallelising the data ingestion phase. See the section on configuring [Insights Cloak](/docs/#/ops/configuration?id=insights-cloak-configuration) for details.
- The efficiency of data aggregation has been improved and emulated queries now push conditions into the bottommost sub-queries for improved performance.
- Error messages now highlight the problematic part of a query.
- `DOW` (synonym for weekday) is supported both as a function and in `EXTRACT`.

### Changes

- The system now requires a license file provided by Aircloak.
- The audit log has been restructured, grouping similar events and showing up to 1000 events per page.
- Debug exports are allowed for failed queries as well as succesful queries.
- `SAMPLE_USERS`-clauses allow fractional values.

## Version 18.1.3

### Bugfixes

- Reduces the chance of integer overflows.

## Version 18.1.2

### Bugfixes

- Fixes cloak crash when creating error message on some invalid queries.
- Fixes cloak crash when aliasing the user id multiple times.
- Fixes alignment of intervals to 6 - 10 months.
- Fixes hanging queries on PostgreSQL data sources.

## Version 18.1.1

### Bugfixes

- The PostgreSQL interface now works with Python (PyGreSQL and Psycopg2) and Java (JDBC PostgreSQL driver) clients.

## Version 18.1.0

### Enhancements

- Performance improvements including: parallel ingestion of data from a database, optimised processing of queries producing a high number of rows, and automatic pruning of unused selected columns.

### Changes

- Improvements to the anonymisation process will cause queries to return different results from what they did in the past.
- The cloak can now make use of cached system memory for running queries. This allows more queries to be run on hosts
  with limited resources.

### Bugfixes

- The system would claim a column didn't exist in a table if it was also selected in a
  subquery and given an alias only differing from the column name in its case.
- Fixed handling of emulated JOINs when identically-named columns were present.
- Properly parse Unicode whitespace characters.
- Fixes incorrect selection of columns for joined emulated queries.

## Version 17.4.0

### New features

- Added beta support for SAP HANA data sources.
- Added the `date_trunc` function.
- Added global and per-user custom settings for displaying numbers in the UI.
- Support for using min/max/median aggregators over date/time value in the top-level query.
- Median is now supported in subqueries. When it is used, emulation will be activated for the query.
- Added `SAMPLE_USERS` clause for reducing the number of users queried.

### Enhancements

- Like patterns can now include an escape string
- More human-friendly rendering of date/time values in the UI.
- Documented `HEX` and `TRIM` functions.
- Support for TCP transport in PostgreSQL server protocol
- Support for cancelling queries over the Postgres Message Protocol interface
- `right`, `bucket` and `round` functions are now natively supported for MongoDB data sets.

### Bugfixes

- Fixed crash in min/max aggregators when there are insufficient data to produce a value.
- Fixed a bug where loading your past query history would not always return the queries you expected.
- Fixed offloading of the string `length` function.
- Fixed offloading and translation of `left` and `substring`, and the `IS NOT NULL` clause for MongoDB data sets.

## Version 17.3.2

### Bugfixes

- Fixed bug where encoded data would not get properly decoded when offloading joins to MongoDB.

## Version 17.3.1

### Bugfixes

- Fixed crash when joining two subqueries over a MongoDB data set.
- Fixed crash in `stddev` aggregate in cases where there are insufficient data to produce a value.
- Fixed crash that could manifest itself when using an alias in an emulated subquery.
- Fixes filtering of data in an intermediate emulated query.
- Fixed bug that duplicated the user id column for projected tables.
- Fixed bug where no tables where shown when creating a new view.
- Fixed crash because of ambiguous column name after a join.

## Version 17.3.0

### New features

- Introducing support for querying Aircloak Insights from Tableau. The support is still experimental.
  Contact Aircloak for help configuring Tableau and your Aircloak Insights installation.
- [A new section](/admin/warnings) appears in the admin interface when the system experiences problems.

### Enhancements

- The performance of all top-level aggregates (with the exception of `MEDIAN`) has been improved.
  Execution times have been cut by 60% and memory consumption by 75% for representative queries.
- Unreliable query results (results with a low number of distinct users) are highlighted in the query interface.
- When available the locations of errors are shown in the query editor.
- Broken views are highlighted as such in the tables and views sidebar.
- Data sources can be given a description to make them easier to keep apart.
- The system guards against changes to users and groups that would leave it without an administrator.
- The [REST API](/docs/#/api/queries?id=canceling-a-query) has been extended to support cancelling queries.

### Changes

- Improvements to the anonymisation mean queries will return different results from what they did in the past.
- When there are too few users to produce a valid aggregate, `SUM`, `SUM_NOISE`, `AVG`, `AVG_NOISE` and `COUNT_NOISE` now return `null`. `COUNT` will under these circumstances return the lowest reportable number: `2`.
- Audit logging can be disabled in the [settings](/admin/settings) section of the admin interface.
- The source code of third party dependencies is available for download from the [third party licenses](/licenses) page.
- Support for `FULL OUTER JOIN`'s has been disabled as it allows simulating `OR` functionality.

## Version 17.2.0

### New features

- Adds an [Activity Monitor](/admin/activity_monitor) that allows an administrator to see all queries running in the system across all analysts, as well as the memory usage across all connected cloaks.
- Show indication of progress when a query is executing.
- Adds an [endpoint for monitoring](/docs/#/ops/monitoring) by external tools.
- Adds a [settings pane](/admin/settings) with the ability to specify the retention time for past queries.

### Enhancements

- Allow unlimited recall of past query history.
- Produces an error message when a data source is configured with an invalid decryption key.
- Cloaks terminate queries when about to run out of memory, rather than crash.
- Ability to speed up schema detection of MongoDB collections by sampling a subset of the data.
- Improved protection of sensitive details in system health data.
- The performance of computing medians in emulated database queries is improved. In representative use cases by more than a factor of two.
- Improved performance by 30% when using `extract_matches` function.
- Allow multiple queries to be run at once in one tab.
- Restore running queries when refreshing/opening a new tab.

### Changes

- Remove charting of errors and automatic detection of axes. Instead it's possible to select which columns to plot.

### Bugfixes

- Fixes usage of dotted names in sub-queries.
- Fixes filtering of rows generated by `extract_matches`.
- Fix data leakage in `min`/`max` aggregators.

## Version 17.1.0

Initial public release.
