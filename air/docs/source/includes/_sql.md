# Aircloak Query Language

To write queries, you can use Aircloak Query Language (AQL). AQL is a subset of standard SQL, implemented to prevent leakage of sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain anonymized data from tables. See [Understanding query results](understanding-query-results) for an explanation of the effects of anonymization on the results.

The syntax conforms to the standard SQL syntax, but only a subset of features is supported. The general shape of the query looks like:

<pre style="float:left; background-color:inherit; color:inherit; text-shadow:inherit; padding-top: inherit;">
  SELECT [DISTINCT]
    field_expression [, ...]
    FROM from_expression [, ...]
    [ WHERE where_expression [AND ...] ]
    [ GROUP BY column_name [, ...] ]
    [ HAVING having_expression [AND ...] ]
    [ ORDER BY column_name [ASC | DESC] [, ...] [ LIMIT amount ] [ OFFSET amount ] ]

  field_expression :=
    column_expression [AS alias]

  column_expression :=
    column_name |
    aggregation_function([DISTINCT] column_name)

  from_expression :=
    table | join

  table :=
    table_name | (select_expression) [AS] alias

  join :=
    table CROSS JOIN table |
    table { [INNER] | { LEFT | RIGHT | FULL } [OUTER] } JOIN table ON where_expression

  aggregation_function :=
    COUNT | SUM | AVG | MIN | MAX | STDDEV | MEDIAN

  where_expression :=
    column_name comparison_operator value |
    column_name IS [NOT] NULL |
    column_name IN (constant [, ...])
    column_name [NOT] LIKE | ILIKE string_pattern

  having_expression :=
      column_expression comparison_operator (value | column_expression)
</pre>

__Notes__:

- The `*` argument can only be provided to the `COUNT` aggregator and it specifies counting rows
  instead of otherwise counting only non-`NULL` values. `NULL` values are ignored by all other aggregators.
- The operator `OR` is currently not supported.
- The operator `NOT` can only be used in the cases mentioned above (`IS NOT NULL`, `NOT LIKE`, and `NOT ILIKE`).
- You can restrict the range of returned rows by a query using the `LIMIT` and/or `OFFSET` clauses, but you need to
 provide the ORDER BY clause to ensure a stable order for the rows.
- Using the `HAVING` clause requires the `GROUP BY` clause to be specified and conditions must not refer to non-aggregated fields.

## JOIN restrictions

To ensure that data can be reliably anonymized, some limitations exist in the `JOIN` part of the query.

Comparison operators `NOT LIKE`, `NOT ILIKE`, and `<>` are not allowed in join conditions (i.e. the `ON ...` part of a `JOIN` expression).

When analysing data across multiple tables, it is required that the data that is joined is all about the same individual.
This can either be achieved by adding a `WHERE`-clause, or in the case of `INNER JOIN`'s and `OUTER JOIN`'s through
a corresponding restriction in the `ON`-clause.

For example, assuming tables `t1` and `t2` both have a user-id columns called `uid`, you would write joins as follows:

- `SELECT c1, c2 FROM t1, t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 CROSS JOIN t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 INNER JOIN t2 ON t1.uid = t2.uid`

Note:

- `OUTER` is automatically implied when you use `LEFT`, `RIGHT` or `FULL` joins. Writing `LEFT OUTER JOIN` is therefore equivalent to writing `LEFT JOIN`
- `INNER` is automatically implied when you use `JOIN` without any other qualifiers. Writing `t1 JOIN t2` is therefore the same as writing `t1 INNER JOIN t2`

## Subquery restrictions

A subquery expression must always select the user-id column. For example, assuming table `t1` with the user-id column called `uid`:

- __Valid__: `SELECT name FROM (SELECT uid, name FROM t1) sq`
- __Invalid__: `SELECT name FROM (SELECT name FROM t1) sq`

Operators `<>`, `IN`, and `NOT` (except `IS NOT NULL`) can't be used in subquery `WHERE` expressions.


## Understanding query results

`SELECT` queries return anonymized results. The results have a small amount of noise added to them. This is crucial in protecting the privacy of individuals, while sufficiently unobtrusive to provide accurate results during normal use.

The results are anonymized in two phases:

1. Low-count filtering
2. Adding noise

### Low-count filtering

In this phase, values which are not associated with a sufficiently large number of distinct users are discarded. For example, consider the query `SELECT first_name FROM users`.

Let's say that the names in the `users` table are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 2
John   | 150
Mary   | 1
Tom    | 2

Since the number of distinct users named Bob, Mary, and Tom is too small, these names won't appear in the final result. In contrast, there is a sufficient number of Alices and Johns, so the result will contain the corresponding rows.

In place of the discarded rows, the `*` rows will be included in the result. All columns of these rows will have the value of `*`. So in this example, the distribution of rows after filtering would be as follows:

Name   | Number of returned rows
------ | ------------------------
Alice  | 100
John   | 150
*      | 5

The number of `*` rows indicates the amount of properties that can't be included in the result. Note that this doesn't represent the number of _distinct_ omitted values. In this example, three distinct names are not reported (Bob, Mary, and Tom), but since there are two Bobs, one Mary, and two Toms, the result contains `2 + 1 + 2 = 5` `*` rows.

It's worth noting that absence of `*` rows doesn't mean that no rows were omitted. The `*` rows have to pass the same anonymization procedure. Thus, if the total count of `*` rows is too low, they will be omitted from the result.

### Adding noise

After low-count values are filtered, some amount of noise is introduced. Consider the example from the [previous section](#low-count-filtering), where there are 100 Alices, 150 Johns, and 5 other names. The final result might contain a slightly different distribution, for example 94 Alice rows, 152 John rows, and 7 `*` rows.

The results of aggregate functions, such as `SUM` and `COUNT`, are also anonymized. The returned values will slightly differ from the real values.

## Date functions

The functions `year`, `month`, `day`, `hour`, `minute`, `second`, and `weekday` are supported. They extract
the named part from a date or time column.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

## Mathematical operators

The operators `+`, `-`, `/`, and `*` have their usual meaning of addition, subtraction, division, and
multiplication respectively. The operator `^` denotes exponentiation. The operator `%` denotes the division
remainder.


```sql
1 - 2 + 4 * 3 / 2
-- 5

2 ^ 3
-- 8

7 % 3
-- 1
```

## Mathematical functions

### abs

Computes the absolute value of the given number.

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

### bucket

Rounds the input to the given bucket size.

```sql
BUCKET(180 BY 50)
-- 150

BUCKET(180 BY 50 ALIGN LOWER)
-- 150

BUCKET(180 BY 50 ALIGN UPPER)
-- 200

BUCKET(180 BY 50 ALIGN MIDDLE)
-- 175
```

### ceil / ceiling

Computes the smallest integer that is greater than or equal to its argument.

```sql
CEIL(3.22)
-- 4
```

### div

Performs integer division on its arguments.

```sql
DIV(10, 2)
-- 5

DIV(10, 3)
-- 3
```

### floor

Computes the largest integer that is less than or equal to its argument.

```sql
FLOOR(3.22)
-- 3
```

### mod

`MOD(a, b)` computes the remainder from `DIV(a, b)`.

```sql
MOD(10, 3)
-- 1
```

### pow

`POW(a, b)` computes `a` to the `b`-th power.

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

### round

Rounds the given floating-point value to the nearest integer. An optional second argument signifies the precision.

```sql
ROUND(3.22)
-- 3

ROUND(3.99)
-- 4

ROUND(3.22, 1)
-- 3.2
```

### sqrt

Computes the square root.

```sql
SQRT(2)
-- 1.4142135623730951
```

### trunc

Rounds the given floating-point value towards zero. An optional second argument signifies the precision.

```sql
TRUNC(3.22)
-- 3

TRUNC(-3.22)
-- -3

TRUNC(3.22, 1)
-- 3.2
```

## String functions

### btrim

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

### concat

Joins the passed strings into one.

```sql
CONCAT('some ', 'text')
-- 'some text'

CONCAT('a', 'b', 'c')
-- 'abc'

'a' || 'b' || 'c'
-- 'abc'
```

### left

`LEFT(string, n)` takes n characters from the beginning of the string. If n is negative takes all but the last |n| characters.

```sql
LEFT('some text', 4)
-- 'some'

LEFT('some text', -2)
-- 'some te'
```

### length

Computes the number of characters in the string.

```sql
LENGTH('some text')
-- 9
```

### lower

Transforms all characters in the given string into lowercase.

```sql
LOWER('Some Text')
-- 'some text'

LCASE('Some Text')
-- 'some text'
```

### ltrim

Removes all of the given characters from the beginning of the string. The default is to remove spaces.

```sql
LTRIM(' some text ')
-- 'some text '

LTRIM('xyzsome textzyx', 'xyz')
-- 'some textzyx'
```

### right

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

### rtrim

Removes all of the given characters from the end of the string. The default is to remove spaces.

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

### substring

Takes a slice of a string.

```sql
SUBSTRING('some text' FROM 3)
-- 'me text'

SUBSTRING('some text' FROM 3 FOR 5)
-- 'me te'

SUBSTRING('some text' FOR 4)
-- 'some'
```

### upper

Transforms all characters in the given string into uppercase.

```sql
UPPER('Some Text')
-- 'SOME TEXT'

UCASE('Some Text')
-- 'SOME TEXT'
```

## Casting

You can convert values between different types using a cast expression.

```sql
CAST('3' AS INTEGER)
-- 3

CAST(3, TEXT)
-- '3'

CAST('NOT A NUMBER', INTEGER)
-- NULL
```

Types can be converted according to the following tables:

|           |      |         |      |         |
|-----------|------|---------|------|---------|
| from\to   | text | integer | real | boolean |
| text      | ✓    | ✓       | ✓    | ✓       |
| integer   | ✓    | ✓       | ✓    | ✓       |
| real      | ✓    | ✓       | ✓    | ✓       |
| boolean   | ✓    | ✓       | ✓    | ✓       |
| date      | ✓    |         |      |         |
| time      | ✓    |         |      |         |
| datetime  | ✓    |         |      |         |
| interval  | ✓    |         |      |         |

|           |      |      |           |          |
|-----------|------|------|-----------|----------|
| from\to   | date | time | datetime  | interval |
| text      | ✓    | ✓    | ✓         | ✓        |
| integer   |      |      |           |          |
| real      |      |      |           |          |
| boolean   |      |      |           |          |
| date      | ✓    |      |           |          |
| time      |      | ✓    |           |          |
| datetime  | ✓    | ✓    | ✓         |          |
| interval  |      |      |           | ✓        |

A cast may fail even when it's valid according to the table. For example a text field may contain data that
does not have the correct format. In that case a `NULL` is returned.

### Casting to/from text

Casting from text will accept the same format as the cast to text produces for the given type. That means:

* `'TRUE'`/`'FALSE'` for booleans
* A base-10 notation for integers
* `1.23` or `1e23` for reals
* ISO-8601 notation for dates, times, datetimes and intervals

### Casting to integer

Casting a real to integer rounds the number to the closests integer.

### Casting to/from boolean

When converting numbers to booleans non-zero numbers are converted to `TRUE` and zero is converted to `FALSE`.
When converting from booleans `TRUE` is converted to `1` and `FALSE` is converted to `0`.

### Casting from datetime

Casting from datetime to date or time will select the date/time part of the datetime.
