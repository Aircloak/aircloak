## Version 18.1.0

### Changes

- Improvements to the anonymization process will cause queries to return different results from what they did in the past.

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

### Bug fixes
- Fixes usage of dotted names in sub-queries.
- Fixes filtering of rows generated by `extract_matches`.
- Fix data leakage in `min`/`max` aggregators.

## Version 17.1.0

Initial public release.
