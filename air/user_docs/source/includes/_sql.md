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
    aggregation_function([DISTINCT] column_name) |
    function(column_expression) |
    column_expression binary_operator column_expression

  binary_operator :=
    + | - | * | / | ^ | %

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
    column_name equality_operator (value | column_name) |
    column_name inequality_operator (numerical_value | datetime_value) |
    column_name BETWEEN value AND value |
    column_name IS [NOT] NULL |
    column_name [NOT] IN (constant [, ...])
    column_name [NOT] LIKE | ILIKE string_pattern

  having_expression :=
      column_expression comparison_operator (value | column_expression)

  comparison_operator :=
      equality_operator | inequality_operator

  equality_operator :=
      = | <>

  inequality_operator :=
      > | >= | < | <=
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

When using `LIMIT` and `OFFSET` in a subquery:

- `LIMIT` will be adjusted to the closest number in the sequence `[10, 20, 50, 100, 200, 500, 1000, ...]` (i.e. 10e^n, 20e^n, 50e^n for any natural number n larger than 0). For example: 1 or 14 become 10, etc
- `OFFSET` will automatically be adjusted to the nearest multiple of `LIMIT`. For example an `OFFSET` of 240 will be
  adjusted to 200 given a `LIMIT` of 100


## Math restrictions

In the following description we use these definitions:

### Discontinuous function

```sql
abs(-10) = 10 -- removes the sign
trunc(1.1043) = 1 -- removes the decimal places
left('hello world', 5) = 'hello' -- removes part of a string
```

A discontinuous function is one that performs an irreversible operation on a column value. An example of such a function
would be `trunc` which takes a floating point value and removes the decimal places. It is discontinuous because there is
no way to get back to the decimal component by applying other functions. Other functions in this family include `left`
which returns part of a string value, `abs` which removes the sign of a number.

Functions that are supported by Aircloak are marked with whether or not they are discontinuous in this documentation.

### Continuous math

```sql
1 + 1 -- is continuous, + can be undone with -
1 * 10 -- is continuous because * can be undone with /
div(5, 2) -- is NOT continuous, because it truncates the result
```

A continuous math function are the normal math operations `+`, `-`, `*` and `pow`. We do not count `/` to this list as
the behaviour is unspecified when dividing by `0`. Likewise, `sqrt` does not belong to this list as it is not defined
for negative numbers.

### Dangerously discontinuous

```sql
abs(age) -- not dangerous
abs(age - 10) -- dangerous because a constant has been involved

left(name, 4) -- ok, despite being discontinuous and with a constant
length(left(name, 4)) -- dangerously discontinuous, because the expression is later cast to a number
```

A dangerously discontinuous function expression is defined as one where one or more of the parameters have been
influenced by a constant.

Discontinuous string functions are only considered dangerous if the resulting expression is later cast to a number.

### Dangerous math

```sql
age + 1 -- dangerous, because a constant was involved
age + height -- not dangerous, no constant
```

We classify math as dangerous if one or more of the parameters have been influenced by a constant.

### Rules

```sql
-- Prohibited because `age` had:
-- - dangerous math applied (addition with a constant), prior to a
-- - dangerously discontinuous function (discontinuous function after
--   the expression had been influenced by the constant 1
SELECT avg(age) FROM (
  SELECT uid, trunc(age + 1) as age
  FROM users
) t

-- Prohibited because `age` has been had math with an integer
-- done to it prior to being used in an WHERE-clause inequality
SELECT avg(age) FROM (
  SELECT uid, age + 1 as age
  FROM users
) t
WHERE age >= 10 and age < 20
```

A column expression that is selected in query cannot have had dangerous math __and__ a dangerously discontinuous function applied
to it.

A column expression that is used in a WHERE-clause inequality (in other words together with `>`, `>=`, `<` or `<=`) cannot have been
through a dangerously discontinuous function __nor__ through a dangerous math operation.

If you run into this restriction in your query, please see if it is possible to change the order of operations in your
query. For example `trunc(age) + 1` is allowed for a column that is to be selected, whereas `trunc(age + 1)` is not. The
reason being that `trunc` applied to the `age`-column is not considered dangerously discontinuous.


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

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

Computes the absolute value of the given number.


### bucket

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

Rounds the input to the given bucket size.


### ceil / ceiling

```sql
CEIL(3.22)
-- 4
```

Computes the smallest integer that is greater than or equal to its argument.


### div

```sql
DIV(10, 2)
-- 5

DIV(10, 3)
-- 3
```

Performs integer division on its arguments.


### floor

```sql
FLOOR(3.22)
-- 3
```

Computes the largest integer that is less than or equal to its argument.


### mod

```sql
MOD(10, 3)
-- 1
```

`MOD(a, b)` computes the remainder from `DIV(a, b)`.


### pow

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

`POW(a, b)` computes `a` to the `b`-th power.


### round

```sql
ROUND(3.22)
-- 3

ROUND(3.99)
-- 4

ROUND(3.22, 1)
-- 3.2
```

Rounds the given floating-point value to the nearest integer. An optional second argument signifies the precision.


### sqrt

```sql
SQRT(2)
-- 1.4142135623730951
```

Computes the square root.


### trunc

```sql
TRUNC(3.22)
-- 3

TRUNC(-3.22)
-- -3

TRUNC(3.22, 1)
-- 3.2
```

Rounds the given floating-point value towards zero. An optional second argument signifies the precision.


## String functions

### btrim

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.


### concat

```sql
CONCAT('some ', 'text')
-- 'some text'

CONCAT('a', 'b', 'c')
-- 'abc'

'a' || 'b' || 'c'
-- 'abc'
```

Joins the passed strings into one.


### left

```sql
LEFT('some text', 4)
-- 'some'

LEFT('some text', -2)
-- 'some te'
```

`LEFT(string, n)` takes n characters from the beginning of the string. If n is negative takes all but the last |n| characters.


### length

```sql
LENGTH('some text')
-- 9
```

Computes the number of characters in the string.


### lower

```sql
LOWER('Some Text')
-- 'some text'

LCASE('Some Text')
-- 'some text'
```

Transforms all characters in the given string into lowercase.


### ltrim

```sql
LTRIM(' some text ')
-- 'some text '

LTRIM('xyzsome textzyx', 'xyz')
-- 'some textzyx'
```

Removes all of the given characters from the beginning of the string. The default is to remove spaces.


### right

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.


### rtrim

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

Removes all of the given characters from the end of the string. The default is to remove spaces.


### substring

```sql
SUBSTRING('some text' FROM 3)
-- 'me text'

SUBSTRING('some text' FROM 3 FOR 5)
-- 'me te'

SUBSTRING('some text' FOR 4)
-- 'some'
```

Takes a slice of a string.


### upper

```sql
UPPER('Some Text')
-- 'SOME TEXT'

UCASE('Some Text')
-- 'SOME TEXT'
```

Transforms all characters in the given string into uppercase.


### extract_match

```sql
EXTRACT_MATCH('Some Text', 'Some')
-- 'Some'

EXTRACT_MATCH('This or that', 'this|that')
-- 'This'

EXTRACT_MATCH('This or that', 'Some')
-- nil
```

Runs a regular expression over a text column. The first match is extracted and replaces the original value.
The syntax of the regular expressions [resemble that of Perl](http://erlang.org/doc/man/re.html#regexp_syntax).

All regular expressions are considered case insensitive.

This function is not allowed in subqueries.

### extract_matches

```sql
EXTRACT_MATCHES('Some Text', '\w+')
-- 'Some'
-- 'Text'

EXTRACT_MATCH('This or that', 'this|that')
-- 'This'
-- 'that'

EXTRACT_MATCH('This or that', 'Some')
# Notice, the row is surpressed when there is no match
```

Runs a regular expression over a text column. All matches are extracted and turned into individual rows.
The syntax of the regular expressions [resemble that of Perl](http://erlang.org/doc/man/re.html#regexp_syntax).

All regular expressions are considered case insensitive.

This function is not allowed in subqueries.

Keep in mind that this function might affect your analysis in unexpected ways.
When a column value is split into multiple values, the whole row is replecated.
This will affect the behaviour of other aggregate functions!

For example, consider the following row coming from the database:

| user-id | price | description |
|---------|-------|-------------|
| 1 | 10.00 | purchase of a book |
| 2 | 15.00 | book of the year |

If used in conjunction with `extract_matches(description, '\w+')`, the input data will be
converted into the following rows before furhter analysis takes place

| user-id | price | description |
|---------|-------|-------------|
| 1 | 10.00 | purchase |
| 1 | 10.00 | of |
| 1 | 10.00 | a |
| 1 | 10.00 | book |
| 2 | 15.00 | book |
| 2 | 15.00 | of |
| 2 | 15.00 | the |
| 2 | 15.00 | year |


## Casting

```sql
CAST('3' AS INTEGER)
-- 3

CAST(3, TEXT)
-- '3'

CAST('NOT A NUMBER', INTEGER)
-- NULL
```

You can convert values between different types using a cast expression.

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
