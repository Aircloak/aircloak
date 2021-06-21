# Restrictions

The Aircloak system imposes restrictions on the query language used in anonymizing queries that go beyond
those imposed by the ANSI SQL standard. The restrictions, outlined below, do not apply to standard queries.
For an overview over the difference between anonymizing and standard queries, consult the
[Query and subquery types](/sql.md#query-and-subquery-types) section.

## JOIN restrictions

To ensure that data can be reliably anonymized, some limitations exist in the `JOIN` part of the query.

Comparison operators `NOT LIKE`, `NOT ILIKE`, and `<>` are not allowed in join conditions (i.e. the `ON ...` part of a `JOIN` expression).

When analysing data across multiple tables, it is required that the data that is joined is all about the same individual.
This can either be achieved by adding a `WHERE`-clause, or in the case of `INNER JOIN`'s and `OUTER JOIN`'s through
a corresponding restriction in the `ON`-clause.

For example, assuming tables `t1` and `t2` both have a `user_id` columns called `uid`, you would write joins as follows:

- `SELECT c1, c2 FROM t1, t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 CROSS JOIN t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 INNER JOIN t2 ON t1.uid = t2.uid`

Note:

- `OUTER` is automatically implied when you use `LEFT`, `RIGHT` joins. Writing `LEFT OUTER JOIN` is therefore equivalent to writing `LEFT JOIN`
- `INNER` is automatically implied when you use `JOIN` without any other qualifiers. Writing `t1 JOIN t2` is therefore the same as writing `t1 INNER JOIN t2`

## Subquery restrictions

A subquery expression with an aggregate must always select a `user_id` column as well. For example, assuming there exists a table `t1` with a `user_id` column called `uid`:

- **Valid**: `SELECT name FROM (SELECT name FROM t1) sq`
- **Valid**: `SELECT name FROM (SELECT uid, count(*) FROM t1 GROUP BY uid) sq`
- **Invalid**: `SELECT name FROM (SELECT count(*) FROM t1) sq`

When using `LIMIT` and `OFFSET` in a subquery:

- `LIMIT` is required if `OFFSET` is specified
- `LIMIT` will be adjusted to the closest number in the sequence `[10, 20, 50, 100, 200, 500, 1000, ...]` (i.e. `10e^n`, `20e^n`, `50e^n`
  for any natural number `n` larger than 0). For example: 1 or 14 become 10, etc
- `OFFSET` will automatically be adjusted to the nearest multiple of `LIMIT`. For example an `OFFSET` of 240 will be
  adjusted to 200 given a `LIMIT` of 100

## Top-level HAVING clause

Any conditions specified in the `HAVING` clause of the top-level query (_not_ a subquery) are "safe" in the sense that
they will only ever be applied to data that has already been aggregated and anonymized. The clause will merely affect
which of the anonymized data to display, not how that data is obtained. Because of this, many of the restrictions
described in the following sections don't apply to the top-level `HAVING` clause.

## CASE statements

`CASE` statements over personal data have multiple restrictions:

- They are only allowed in the `SELECT` or `GROUP BY` clauses of anonymizing queries;
- They can not be post-processed in any way, other than aggregation;
- The `WHEN` clauses can only consist of a single equality condition between a clear expression and a constant.
  The constant has to be from the list of frequent values in that column, unless the system administrator explicitly
  allows usage of any value. Check the
  [Insights Cloak configuration](/ops/configuration.md#insights-cloak-configuration) section for information
  on how to enable it.
- The `THEN`/`ELSE` clauses can only return constant values; furthermore, when aggregated, they can only return
  the values 0, 1 or NULL.

A few examples:

```sql
-- Correct - conditional selection and grouping:
SELECT
  CASE
    WHEN column = 'aaa' THEN 1
    WHEN column = 'bbb' THEN 2
    ELSE NULL
  END,
  COUNT(*)
FROM table
GROUP BY 1

-- Correct - conditional aggregation:
SELECT SUM(CASE WHEN column = 'aaa' THEN 1 END) FROM table

-- Incorrect - multiple conditions are used in the same `WHEN` clause:
SELECT CASE WHEN column = 'aaa' AND column = 'bbb' THEN TRUE END FROM table

-- Incorrect - an unsupported condition is used in the `WHEN` clause:
SELECT CASE WHEN column <> 'aaa' THEN TRUE END FROM table

-- Incorrect - the `THEN` clause doesn't return a constant value:
SELECT CASE WHEN column = 'aaa' THEN other_column END FROM table

-- Incorrect - the `ELSE` clause returns an unsupported constant during aggregation:
SELECT AVG(CASE WHEN column = 'aaa' THEN 0 ELSE 1000 END) FROM table

-- Incorrect - `CASE` statement is not used in the `SELECT` or `GROUP BY` clauses:
SELECT COUNT(*) FROM table WHERE CASE WHEN column = 3 THEN TRUE ELSE FALSE END

```

## Math and function application restrictions

The usage of some functions is restricted. The scenarios when the restrictions come into effect are when a database
column is transformed by more than 5 such functions and the expression on which the functions operate also contains a constant.

An expression containing two or more mathematical operators is considered to be the equivalent of a constant.
The reason for this is that one can easily construct constants from pure database columns.
For example `pow(colA, colB - colB)` equals the number 1.

The rules apply to the following functions:

- `abs`, `bucket`, `ceil`, `floor`, `length`, `round`, `trunc`, and `cast`'s.
- `+`, `-`, `*`, `/`, `^`, `%`, `pow`, `sqrt`
- `year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, `weekday`, `date_trunc`
- `btrim`, `ltrim`, `rtrim`, `left`, `right`, `substring`

Below is an example of the restrictions in action:

```sql
-- The following query contains more than 5 restricted functions as well as a constant and
-- is therefore rejected.

SELECT
  -- This expression contains a total of 7 restricted functions:
  -- - 3 from value1
  -- - 3 from value2
  -- - 1 from the addition of value1 and value2
  value1 + value2
FROM (
  SELECT
    uid,
    -- contains 3 restricted functions, namely:
    -- - division with a constant
    -- - abs on an expression containing a constant
    -- - + where one of the arguments is an expression containing a constant
    abs(age / 2) + height as value1
  FROM table1
) a INNER JOIN (
  SELECT
    uid,
    -- contains 3 restricted functions, namely:
    -- - addition with a constant
    -- - division with a constant
    -- - multiplication where one of the arguments is an expression containing a constant
    (birth_year + 1) / 11 * height as value2
  FROM table
) b ON a.uid = b.uid
```

Below is an example of a query being rejected because multiple math operators have been interpreted as being a constant:

```sql
SELECT
  -- we have a total of 6 functions operating on an expression containing a potential constant,
  -- as a result the query is rejected.
  floor(abs(sqrt(ceil(floor(sqrt(
    -- Aircloak considers two or more math operations to potentially be a constant
    (age / age) / age
  , 2)), 2))))
FROM table
```

Functions that can cause database exceptions when a database column contains a certain value are
prohibited. These functions include division and sqrt when the divisor and the parameter respectively are
expressions containing a database column as well as a constant value.

Below is an example of the restrictions in action:

```sql
-- The following query is illegal as the divisor contains a constant, in this case the number 1
SELECT age / (age + 1) FROM table
```

## Constant values

In order to prevent overflow errors, the following restrictions on constant values are in place:

- Numeric values are limited to the range `[-10^18, 10^18]`.
- Date and datetime years are limited to the range `[1900, 9999]`.
- Intervals are limited to `100` years.

## Clear expressions

A clear expression is a simple expression that:

- references exactly one database column,
- uses at most one `CAST`,
- only uses the following allowed functions:
  - string functions: `lower`, `upper`, `substring`, `trim`, `ltrim`, `rtrim`, `btrim`, `hex`, `left`, `right`;
  - date/time functions: `year`, `quarter`, `month`, `weekday`, `day`, `hour`, `minute`, `second`, `date_trunc`;
  - numerical functions: `trunc`, `floor`, `ceil`, `round`, `bucket`;
  - any aggregator (`MIN`, `MAX`, `COUNT`, `SUM`, `AVG`, `STDDEV`, `VARIANCE`).

Such expressions are considered to be safe in general and are exempt from many of the following restrictions.

### Aggregated expressions

All aggregated expressions have to be clear.

```sql
-- Correct - aggregated expression is clear:
SELECT SUM(round(column)) FROM table

-- Incorrect - aggregated expression is not clear:
SELECT SUM(1 / column) FROM table
```

### `IS [NOT] NULL` conditions

The subject of an `IS [NOT] NULL` condition has to be a clear expression.

```sql
-- Correct - subject is a clear expression:
SELECT COUNT(*) FROM table WHERE column IS NOT NULL

-- Incorrect - subject is not a clear expression:
SELECT COUNT(*) FROM table WHERE 1 / column IS NULL
```

## Constant ranges

Whenever a comparison (`>`, `>=`, `<`, or `<=`) with a constant is used in a `WHERE`-, `JOIN`- or `HAVING`-clause,
that clause needs to contain two comparisons. These should form a constant range on a single clear expression.
That is, one `>=` comparison and one `<` comparison, limiting the expression from bottom and top.

The following special cases are excluded from this restriction:

- comparisons with clear expressions on both sides;
- date comparisons between a clear expression and the current date.
- date comparisons between a constant, month-aligned date and a range of clear expressions.

```sql
-- Correct - a constant range is used:
SELECT COUNT(*) FROM table WHERE column > 10 AND column < 20

-- Correct - comparison between clear expressions:
SELECT COUNT(*) FROM table WHERE column1 > column2
SELECT COUNT(*) FROM table WHERE column1 < round(column2)

-- Incorrect - only one side of the constant range provided:
SELECT COUNT(*) FROM table WHERE column > 10

-- Incorrect - the lower end of the constant range is bigger than the upper end:
SELECT COUNT(*) FROM table WHERE column > 10 AND column < 0

-- Incorrect - the comparisons are over different expressions:
SELECT COUNT(*) FROM table WHERE column + 1 > 10 AND column - 1 < 20

-- Incorrect - multiple columns are referenced on one side of the comparison:
SELECT COUNT(*) FROM table WHERE column1 - column1 < column2

-- Correct - comparison between a clear expression and the current date:
SELECT COUNT(*) FROM table WHERE column <= current_date()

-- Correct - comparison between a month-aligned date and a range of clear expressions:
SELECT COUNT(*) FROM table WHERE date '2020-01-01' BETWEEN column1 AND column2
```

Note that a condition using the `BETWEEN` operator automatically forms a constant range:

```sql
-- These two queries are equivalent:
SELECT COUNT(*) FROM table WHERE column BETWEEN 10 AND 20
SELECT COUNT(*) FROM table WHERE column >= 10 AND column < 20
```

## Constant range alignment

The system will adjust constant ranges provided in queries. The adjustment will "snap" the range to a fixed, predefined
grid. It will always make sure that the specified range is included in the adjusted range. The range will also be
modified to be closed on the left (`>=`) and open on the right (`<`).

If any such modifications take place an appropriate notice will be displayed in the web interface. When using the API
the notice will be included under the `info` key of the result. When using the PostgreSQL interface, it will be sent
across the wire as a `notice` message.

The grid sizes available depend on the type of the column that is being limited by the range:

- For numerical columns the grid sizes are `[..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, ...]`
- For date/time columns they are:
  - `[1, 2, 3, 6, 9, 12, ...]` months
  - `[1, 2, 5, 10, 15, 20]` days
  - `[1, 2, 6, 12]` hours
  - `[1, 2, 5, 15, 30]` minutes
  - `[1, 2, 5, 15, 30]` seconds.

The adjusted range will have the smallest size from the ones listed that can contain the full range provided in the
query. Furthermore the starting point of the range will be changed so that it falls on a multiple of the adjusted
range's size from a zero point. That zero point is the number 0 for numbers, midnight for times, and
`1900-01-01 00:00:00` for dates and datetimes.

To better fit the range provided in the query the range might also be shifted by half its size, however this will not
happen in the following cases:

- A range of 1 day on a date type - the underlying data type cannot represent such a shift
- A range of 1 second on a time or datetime type - the underlying data type cannot represent such a shift
- A range of 1 month - months have an irregular number of days and no clear "half-point"

For best results design your queries so that they take this adjustment into account and mostly use ranges that are
already adjusted.

```sql
SELECT COUNT(*) FROM table WHERE column > 10 AND column < 20
-- Adjusted to 10 <= column < 20

SELECT COUNT(*) FROM table WHERE column >= 10 AND column < 19
-- Adjusted to 10 <= column < 20

SELECT COUNT(*) FROM table WHERE column >= 9 AND column < 19
-- Adjusted to 0 <= column < 20

SELECT COUNT(*) FROM table WHERE column >= 16 AND column < 24
-- Adjusted to 15 <= column < 25

SELECT COUNT(*) FROM table WHERE date >= '2016-01-01' AND date < '2016-01-29'
-- Adjusted to a full month - 2016-01-01 <= date < 2016-02-01

SELECT COUNT(*) FROM table WHERE datetime >= '2016-01-01 12:27:00' AND date < '2016-01-01 12:31:00'
-- Adjusted to a grid size of 5 minutes - 2016-01-01 12:22:30 <= datetime < 2016-01-01 12:37:30
-- The 5 minute intervals can start on a full five-minute mark or a 2 minutes 30 seconds mark

SELECT COUNT(*) FROM table WHERE date >= '2017-01-10' AND date < '2017-01-20'
-- Adjusted to 20 days - '2017-01-07' <= date < '2017-01-27'
-- The day-sized intervals can only start a multiple of their size from 1900-01-01

SELECT COUNT(*) FROM table WHERE date >= '2017-01-07' AND date < '2017-01-17'
-- Not adjusted -- see previous example
```

## Implicit ranges

Some functions can be used to almost the same effect as a pair of inequalities. For example the following two queries
are roughly equivalent:

```sql
SELECT COUNT(*) FROM table WHERE round(number) = 10
SELECT COUNT(*) FROM table WHERE number >= 9.5 AND number < 10.5
```

Because of this, usage of such functions must be restricted in a similar way to inequalities and the `BETWEEN` operator.
The restrictions disallow the usage of most functions or mathematical operations before or after applying an implicit
range function, if the expression is not clear. The operations that can be applied are a single `CAST`, any aggregator
(`MIN`, `MAX`, `COUNT`, `SUM`, `AVG`, `STDDEV`, `VARIANCE`), and a sub-month date extraction function (`day`,
`weekday`, `hour`, `minute`, `second`, `extract(day/weekday/hour/minute/second)`). The restrictions apply when an
implicit range function is used in a `WHERE` or `JOIN` clause, selected in the top-level `SELECT` clause or used in a
non-top-level `HAVING` clause - see [Top-level HAVING clause](#top-level-having-clause).

The following functions are treated as implicit range functions: `round`, `trunc`, `date_trunc`, and sub-month date
extraction functions (`day`, `weekday`, `hour`, `minute`, `second`, `extract(day/weekday/hour/minute/second)`).

```sql
-- Correct - no other function used
SELECT COUNT(*) FROM table WHERE round(number) = 10

-- Correct - an aggregate is used
SELECT COUNT(*) FROM table GROUP BY category WHERE round(max(number)) = 10

-- Incorrect - another operation (/) is applied in the same expression as round
SELECT COUNT(*) FROM table WHERE round(number / 2) = 10

-- Correct - used in the top-level HAVING, so restrictions don't apply
SELECT COUNT(*) FROM table GROUP BY category HAVING round(max(number) / 2) = 10

-- Incorrect - used in a non-top-level HAVING
SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY category HAVING round(max(number) / 2) = 10) x

-- Incorrect - another operation is used in top-level SELECT
SELECT round(abs(number)) FROM table

-- Correct - math on month-aligned expressions is allowed
SELECT COUNT(*) FROM table WHERE year(birthday) * 12 + month(birthday) = 2000 * 12 + 3

-- Incorrect - math on sub-month-aligned expressions is rejected
SELECT COUNT(*) FROM table WHERE month(birthday) * 30 + day(birthday) = 100
```

## Text operations

Certain operations on textual data can be used to almost the same effect as a pair of inequalities. For example the
following two queries are roughly equivalent:

```sql
SELECT COUNT(*) FROM table WHERE number BETWEEN 10 AND 20

SELECT COUNT(*) FROM table WHERE
  LEFT(CAST(number AS text), 1) = '1' AND length(CAST(number AS text)) = 2
```

Because of this, the usage of operations on textual data has to be restricted to prevent circumvention of measures that
would normally limit what can be done with range conditions. The restrictions on expressions containing text
manipulation functions are the same as ones described for [implicit ranges](#implicit-ranges). In addition, the result of
text manipulation can only be compared to a clear expression.

The following functions are treated as text manipulation functions: `left`, `right`, `rtrim`, `ltrim`, `trim`, and
`substring`.

```sql
-- Correct
SELECT COUNT(*) FROM table WHERE LEFT(name, 1) = 'A'

-- Incorrect - the results of a text operation are compared to a complex expression
SELECT COUNT(*) FROM table WHERE LEFT(name, 1) = RIGHT(name || 'a', 1)
```

Furthermore, the aggregators `min` and `max` cannot be used on data of type `text` in anonymizing queries and
subqueries (see [Query and subquery types](/sql.md#query-and-subquery-types) for more about this distinction). This is
due to the mode of operation of these aggregators and the fact that they require estimating the spread in subsets of the
data. See [Aggregates](/sql/query-results.md#anonymizing-aggregation-functions) for more on this topic.

## IN, NOT IN, NOT LIKE, and <>

Any conditions using `IN`, `NOT LIKE`, `NOT ILIKE`, or `<>` are subject to additional restrictions. Note that `NOT IN`
is treated just as `<>`, because there is always an equivalent query using `<>` for every `NOT IN` query:

```sql
-- These queries are equivalent
SELECT COUNT(*) FROM table WHERE number NOT IN (1, 2, 3)
SELECT COUNT(*) FROM table WHERE number <> 1 AND number <> 2 AND number <> 3
```

### Allowed expressions

Conditions using `IN` or `<>` have to have a clear expression on the left-hand side.
Only a single `COUNT`, `MIN` or `MAX` aggregator is allowed in such conditions.

All items on the right-hand side of the `IN` operator must be constants from the list of frequent values
in that column, unless the system administrator explicitly allows usage of any value. Check the
[Insights Cloak configuration](/ops/configuration.md#insights-cloak-configuration) section for information
on how to enable it.

The right-hand side of a `<>` condition has to be a clear expression
or a constant from the list of frequent values in that column.

Conditions using `NOT LIKE` or `NOT ILIKE` cannot contain any functions except for aggregators.
A single `CAST` is allowed.

The top-level `HAVING` clause is exempt from all these restrictions - see [Top-level HAVING clause](#top-level-having-clause).

```sql
-- Correct - assuming 'alice' is a frequent value
SELECT COUNT(*) FROM table WHERE lower(name) <> 'alice'
SELECT COUNT(*) FROM table WHERE name IN ('alice', 'bob')

-- Incorrect - a disallowed operation was used
SELECT COUNT(*) FROM table WHERE length(name) <> 2
SELECT COUNT(*) FROM table WHERE length(name) IN (1, 2, 3)

-- Correct - top-level HAVING is exempt from restrictions
SELECT COUNT(*) FROM table GROUP BY name HAVING length(name) <> 2

-- Correct - comparing two clear expressions
SELECT COUNT(*) FROM table WHERE name <> surname
SELECT COUNT(*) FROM table WHERE round(column1) <> round(column2)

-- Incorrect - multiple columns are referenced on one side of the inequality:
SELECT COUNT(*) FROM table WHERE column1 - column1 <> column2
```

### Negative conditions over rare values

By default, negative conditions (`NOT IN`, `NOT LIKE`, `NOT ILIKE`, and `<>`) over rare personal values are forbidden.
Conditions that match values appearing frequently in a given column are excluded from this limitation.

This limitation can be relaxed, on a per data source basis, by increasing the value of the `max_rare_negative_conditions`
configuration setting. Check the [Insights Cloak configuration](/ops/configuration.md#insights-cloak-configuration)
section for information on how to modify it. Note that a `NOT IN` condition will be counted multiple times - once for each
rare element on the right-hand side.

The examples below assume that the Insights Cloak is configured to allow one negative condition at the most, and that
the names `Alice` and `Bob` appear frequently in the column `name`, while all other values appear only rarely.

```sql
-- Allowed - only one negative condition matches a rare value
SELECT COUNT(*) FROM table
WHERE name <> 'Alice' AND name <> 'Bob' AND name <> 'Charles'

-- Allowed - there is only one negative conditions on a rare value
SELECT COUNT(*) FROM table
WHERE name <> 'Charles'

-- Disallowed - there are three negative conditions matching rare values
SELECT COUNT(*) FROM table
WHERE name <> 'Charles' AND name <> 'Damien' AND name <> 'Ecbert'

-- Disallowed - equivalent to the previous query
SELECT COUNT(*) FROM table
WHERE name NOT IN ('Charles', 'Damien', 'Ecbert')

-- Allowed - all conditions match frequent values
SELECT COUNT(*) FROM table
WHERE name NOT LIKE 'A%' AND name NOT LIKE 'B%' AND upper(name) <> 'BOB'
```

## Isolating columns

Some columns in the underlying data source might identify users despite not being marked as a user id. For example a
table might contain a `user_id` column and an `email` column. The emails are in all likelihood unique per user, and so
can identify a user just as well as the `user_id` column. We call these columns "isolating" and apply some additional
restrictions to expressions including them. Note that the `user_id` column is always isolating.

Only clear expressions are allowed on these columns. All other functions and mathematical operations are forbidden.

Furthermore, conditions using the `LIKE` operator are limited to simple patterns of the form `%foo`, `foo%`, or `%foo%`.

```sql
-- These examples assume that the 'email' and 'social_security' columns are isolating

-- Correct
SELECT COUNT(*) FROM table WHERE trim(email) = 'alice@example.com'

-- Correct
SELECT COUNT(*) FROM table WHERE email <> 'alice@example.com'

-- Incorrect - a function from outside the allowed list is used
SELECT COUNT(*) FROM table WHERE length(email) = 20

-- Incorrect - a mathematical operation is used
SELECT COUNT(*) FROM table WHERE social_security / 10 = 10000000

-- Correct
SELECT COUNT(*) FROM table WHERE BUCKET(social_security BY 10) = 100000000

-- Incorrect - a disallowed LIKE pattern is used
SELECT COUNT(*) FROM table WHERE email LIKE 'alice@%.com'

-- Correct
SELECT COUNT(*) FROM table WHERE email LIKE 'alice@example.%'
```

Insights Cloak will automatically discover which columns isolate users. This computation might be very slow and
resource-intensive for large data sources. See [Manually classifying isolating
columns](/ops/configuration.md#manually-classifying-isolating-columns) for information on alternative means of
classifying isolating columns.

## Column bounds

Some mathematical operations can cause an overflow and result in an error when performed in the database. To avoid such
cases, Insights Cloak analyzes the query and finds potentially problematic operations, making sure that either:

- The input columns to these operations have data distributed in such a way that there won't be a problem. For example,
  dividing by a column that is always positive will never result in a divide-by-zero error.
- The operations are performed using a potentially slower but safe method.

Insights Cloak needs the lower and upper bounds of the values in numeric columns in order to perform this analysis.
Note that the actual bounds used by Insights Cloak will be based on the true bounds, but anonymized. Note also, that any
data found outside of these anonymized bounds will be treated as if it had the maximum/minimum anonymized value instead.
The bounds are computed with some "extra room", so this can most often happen in the case of a value being an extreme
outlier.

So long as this analysis is not complete for a certain column, mathematical operations on that column need to be
performed using the safe method, which might be slower on some data sources. In Oracle DB these operations are emulated by default. However, the database administrator can enable
Aircloak UDFs to avoid this emulation - [see here for details](/datastores.md#oracle-safe-functions).

## Column analysis

In order to apply the restrictions described in [Number of conditions](#number-of-conditions), [Isolating
columns](#isolating-columns), and [Column bounds](#column-bounds), Insights Cloak needs to analyze the contents of the
data source. This process might take some time, but the data source is available for querying while the analysis is
under way. While the analysis is incomplete Insights Cloak needs to make conservative assumptions about the data. As a
result, all columns are treated as if they were isolating and had no frequent values, until the analysis is completed
for a particular column.

You can check the isolator status of a table by using the `SHOW COLUMNS` statement:

```sql
SHOW COLUMNS FROM users

| name       | type    | isolator? | comment |
| ---------- | ------- | --------- | ------- |
| uid        | integer | true      |         |
| first_name | text    | false     |         |
| last_name  | text    | true      |         |
| email      | text    | pending   |         |
```

In this case the columns `uid` and `last_name` are isolating, while the column `first_name` is not. The status of the
`email` column is not yet known, so it will be treated as isolating until its analysis is complete.
