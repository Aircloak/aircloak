# Aircloak Query Language

To write queries, you can use Aircloak Query Language (AQL). AQL is a subset of standard SQL, implemented to prevent leakage of sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain anonymized data from tables. See [Understanding query results](understanding-query-results) for an explanation of the effects of anonymization on the results.

The syntax conforms to the standard SQL syntax, but only a subset of features is supported. The general shape of the query looks like:

<pre style="float:left; background-color:inherit; color:inherit; text-shadow:inherit; padding-top: inherit;">
  SELECT
    column_expression [, ...]
    FROM from_expression [, ...]
    [ WHERE where_expression [AND ...] ]
    [ GROUP BY column_name [, ...] ]
    [ ORDER BY column_name [ASC | DESC] [, ...] ]

  column_expression :=
    column_name |
    aggregation_function([DISTINCT] column_name)

  from_expression :=
    table_name |
    table1 CROSS JOIN table2 |
    table1 { [INNER] | { LEFT | RIGHT | FULL } [OUTER] } JOIN table2 ON where_expression

  aggregation_function :=
    COUNT | SUM | AVG | MIN | MAX | STDDEV | MEDIAN

  where_expression :=
    column_name comparison_operator value |
    column_name IS [NOT] NULL |
    column_name IN (constant [, ...])
    column_name [NOT] LIKE | ILIKE string_pattern
</pre>

__Notes__:

- The `*` argument and the `DISTINCT` modifier can only be provided to the `COUNT` function.
- The operator `OR` is currently not supported.
- The operator `NOT` can only be used in the cases mentioned above (`IS NOT NULL`, `NOT LIKE`, and `NOT ILIKE`).

## JOIN restrictions

When analysing data across multiple tables, it is required that the data that is joined is all about the same individual.
This can either be achieved by adding a `WHERE`-clause, or in the case of `INNER JOIN`'s and `OUTER JOIN`'s through
a corresponding restriction in the `ON`-clause.

This requirement is in place to ensure that data can be reliably anonymized.

For example, assuming tables `t1` and `t2` both have a user-id columns called `uid`, you would write joins as follows:

- `SELECT c1, c2 FROM t1, t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 CROSS JOIN t2 WHERE t1.uid = t2.uid`
- `SELECT c1, c2 FROM t1 INNER JOIN t2 ON t1.uid = t2.uid`

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

## Database functions

AQL supports a subset of common database functions to make working with data easier.

### Date functions

The functions `year`, `month`, `day`, `hour`, `minute`, `second`, and `weekday` are supported. They extract
the named part from a date or time column.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

### abs

Computes the absolute value of the given number.

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
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
