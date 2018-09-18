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

### __Breaking change__

- ODBC drivers for SAP HANA must be provided by the client and mounted.
  See the [Installation guide](docs/ops/installation.html) for more info.

### Features

- Added support for userless tables and non-anonymizing queries.
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
- The cloak classifies columns by whether they isolate users or not, further improving the anonymization as a result.
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
- Query execution times can be reduced by parallelising the data ingestion phase. See the section on configuring [Insights Cloak](/docs/ops/configuration.html#insights-cloak-configuration) for details.
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

- Improvements to the anonymization process will cause queries to return different results from what they did in the past.
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
- The [REST API](/docs#canceling-a-query) has been extended to support cancelling queries.

### Changes

- Improvements to the anonymization mean queries will return different results from what they did in the past.
- When there are too few users to produce a valid aggregate, `SUM`, `SUM_NOISE`, `AVG`, `AVG_NOISE` and `COUNT_NOISE` now return `null`. `COUNT` will under these circumstances return the lowest reportable number: `2`.
- Audit logging can be disabled in the [settings](/admin/settings) section of the admin interface.
- The source code of third party dependencies is available for download from the [third party licenses](/licenses) page.
- Support for `FULL OUTER JOIN`'s has been disabled as it allows simulating `OR` functionality.

## Version 17.2.0

### New features

- Adds an [Activity Monitor](/admin/activity_monitor) that allows an administrator to see all queries running in the system across all analysts, as well as the memory usage across all connected cloaks.
- Show indication of progress when a query is executing.
- Adds an [endpoint for monitoring](/docs#monitoring) by external tools.
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
