# Query Language

To write queries you use SQL. Aircloak supports a subset of standard SQL, implemented in a way that prevents leakage of sensitive data.


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
    [ GROUP BY column_expression | position [, ...] ]
    [ HAVING having_expression [AND ...] ]
    [ ORDER BY column_name [ASC | DESC] | position [, ...] [ LIMIT amount ] [ OFFSET amount ] ]

  field_expression :=
    * | table_name.* | column_expression [AS alias]

  column_expression :=
    [table_name.]column_name |
    aggregation_function([DISTINCT] column_name) |
    function(column_expression) |
    column_expression binary_operator column_expression |
    column_expression::data_type

  binary_operator :=
    + | - | * | / | ^ | %

  data_type :=
    integer | real | text | boolean | datetime | date | time

  from_expression :=
    table | join

  table :=
    table_name [[AS] alias] | (select_expression) [AS] alias

  join :=
    table CROSS JOIN table |
    table { [INNER] | { LEFT | RIGHT | FULL } [OUTER] } JOIN table ON where_expression

  aggregation_function :=
    COUNT | SUM | AVG | MIN | MAX | STDDEV | MEDIAN

  where_expression :=
    column_expression equality_operator (value | column_expression) |
    column_expression inequality_operator (numerical_value | datetime_value) |
    column_expression BETWEEN value AND value |
    column_expression IS [NOT] NULL |
    column_expression [NOT] IN (constant [, ...])
    column_expression [NOT] LIKE | ILIKE string_pattern

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
- Conditions in the `HAVING` clause must not refer to non-aggregated fields.
- Aliases can be used in the `WHERE`, `GROUP BY`, `ORDER BY` and `HAVING` clauses, as long as the alias doesn't conflict
 with a column name in one of the selected tables.
- If an integer is specified in the `GROUP BY` clause, it represents a 1-based position in the select list. The corresponding expression from the select list is used as the grouping expression.

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

- `LIMIT` is required if `OFFSET` is specified
- `LIMIT` will be adjusted to the closest number in the sequence `[10, 20, 50, 100, 200, 500, 1000, ...]` (i.e. 10e^n, 20e^n, 50e^n for any natural number n larger than 0). For example: 1 or 14 become 10, etc
- `OFFSET` will automatically be adjusted to the nearest multiple of `LIMIT`. For example an `OFFSET` of 240 will be
  adjusted to 200 given a `LIMIT` of 100


## Math and function application restrictions


```sql
-- The following examples show expressions that are not allowed
-- for column expressions that are selected:

-- both abs and + run on a value in combination with a constant
abs(age + 1)

-- string function used in combination with a constant value,
-- and later part of a math expression with a constant.
length(btrim(name, 'constant')) + 1

-- both sides of the math expression are values that have been
-- processed with a constant value. This expression is
-- forbidden because the columns, in addition to the math have
-- been processed by a discontinuous string function together with
-- a constant value.
length(btrim(first_name, 'constant')) + length(btrim(last_name, 'constant'))


-- The following examples show expressions that are allowed
-- for column expressions that are selected, but not allowed in
-- filter condition inequality clauses

-- restricted function with a constant
length(btrim(name, 'constant'))

-- math with a constant
age * 10


-- The following examples show expressions that are allowed
-- for column expressions that are selected, but not allowed in
-- a filter condition irrespective of whether they are match conditions
-- or inequality conditions, because they extract parts of a date or time

-- cast of a date
left(cast(date as text), 7)

-- extract part of a date
year(date)


-- The following show examples of the restricted functions which are OK
-- both in column expressions that are selected as well as filtering
-- clauses, despite being complex. The reason is that there are no
-- constants involved

length(btrim(firstname, lastname)) + age
length(cast(salary + salary / age as text))
```

Aircloak applies some restrictions on how certain functions and math operators can be used in your queries __when
they are used together with constant values__.
As an example consider the function `btrim`. It can always be used directly on a column expression (for example `btrim(name)`),
but it's usage is restricted when a constant is involved (for example `btrim(name, 'some constant')`).

The restrictions are as follows:

- you cannot _select a column_ in your query if the column has been processed by a restricted function in conjunction with a constant __and__
  there has been performed math with a constant on the column as well
- you cannot use a column in a filter condition clause inequality (meaning `>`, `>=`, `<`, or `<=` in a `WHERE`-, `JOIN`- or `HAVING`-clause)
  if it has had math with a constant performed on it __or__ if it has been processed by one of the restricted functions together with a constant
- you cannot use the result of a cast or of applying a date or time extraction function (like `year`, `hour` etc)
  on a `date`, `time` or `datetime` column in a filter condition clause (neither match nor inequality clause).

The numerical functions that receive this kind of special treatment are: `abs`, `bucket`, `ceil`, `div`, `floor`, `mod`, `round`, `sqrt`, `/`, `trunc`, and `cast`'s.

The following string functions receive this kind of special treatment only if they are later converted to a number:
`btrim`, `left`, `ltrim`, `right`, `rtrim`, and `substring`.

The same applies to the following math operations if one or more of their arguments are a constant or is the result of
a column having been processed together with a constant:
`+`, `-`, `*`, `/`, `^`, `pow`.

The following date and time functions:
`year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, `weekday`

For examples see the sidebar.

## Inequality restrictions

### Ranges

```sql
-- Correct - a range is used
SELECT COUNT(*) FROM table WHERE column > 10 AND column < 20

-- Incorrect - only one side of the range provided
SELECT COUNT(*) FROM table WHERE column > 10

-- Incorrect - the lower end of the range is bigger than the upper end
SELECT COUNT(*) FROM table WHERE column > 10 AND column < 0

-- Incorrect - the inequalities are over different expressions
SELECT COUNT(*) FROM table WHERE column + 1 > 10 AND column - 1 < 20
```

Whenever an inequality (`>`, `>=`, `<`, or `<=`) is used in a `WHERE`-, `JOIN`- or `HAVING`-clause that clause actually needs to contain two
inequalities. These should form a range on a single column or expression. That is, one `>` or `>=` inequality and one `<` or `<=`
inequality, limiting the column/expression from bottom and top.

### Range alignment

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
-- Adjusted to 2016-01-01 <= date < 2016-02-01

SELECT COUNT(*) FROM table WHERE datetime >= '2016-01-01 12:27:00' AND date < '2016-01-01 12:31:00'
-- Adjusted to 2016-01-01 12:22:30 <= datetime < 2016-01-01 12:37:30
```

The system will adjust ranges provided in queries. The adjustment will "snap" the range to a fixed, predefined grid. It will always
make sure that the specified range is included in the adjusted range. The range will also be modified to be closed on the left (`>=`)
and open on the right (`<`).

If any such modifications take place an appropriate notice will be displayed in the web interface. When using the API the notice will
be included under the `info` key of the result. The notice will _not_ appear when using the Postgres interface.

The grids available depend on the type of the column that is being limited by the range. For numerical columns the grid sizes are
`[..., 0.1, 0.2, 0.5, 1, 2, 5, 10, ...]`. For date/time columns they are `[1, 2, 5, ...]` years, `[1, 2, 6, 12]` months, `[1, 2, 5, ...]` days,
`[1, 2, 6, 12, 24]` hours, `[1, 2, 5, 15, 30, 60]` minutes, and `[1, 2, 5, 15, 30, 60]` seconds.

To arrive at the final range the system finds the smallest grid size that will contain the given range. Then it shifts the lower end of the
range to be a multiple of half of the grid size. The upper end is just the lower end plus the grid size. In some cases halving the grid size
is not allowed and the lower end needs to be a multiple of the whole grid size instead - for example it is not allowed to halve days in
case the underlying data type is `date` and cannot represent such halving. See the sidebar for examples of adjustment.

For best results design your queries so that they take this adjustment into account and mostly use ranges that are already adjusted.

## Understanding query results

`SELECT` queries return anonymized results. The results have noise added to them. This is crucial in protecting the privacy of individuals, while sufficiently unobtrusive to provide accurate results during normal use.

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

`*` rows are included in the result in place of rows that are discarded due to anonymization. In this example the distribution of rows after filtering would be:

Name   | Number of returned rows
------ | ------------------------
Alice  | 100
John   | 150
*      | 5

The number of `*` rows indicates the amount of properties that can't be included in the result. Note that this does not represent the number of _distinct_ omitted values. In this example, three distinct names are not reported (Bob, Mary, and Tom), but since there are two Bobs, one Mary, and two Toms, the result contains `2 + 1 + 2 = 5` `*` rows.

It is worth noting that absence of `*` rows doesn't mean that no rows were omitted. The `*` rows have to pass the same anonymization procedure. Thus, if the total count of `*` rows is too low, they will be omitted from the result.

### Adding noise

After low-count values are filtered, some amount of noise is introduced. Consider the example from the [previous section](#low-count-filtering), where there are 100 Alices, 150 Johns, and 5 other names. The final result might contain a slightly different distribution, for example 94 Alice rows, 152 John rows, and 7 `*` rows.

The results of aggregate functions, such as `SUM` and `COUNT`, are also anonymized. The returned values will slightly differ from the real values.

To ensure anonymity the amount of noise added depends on the number and types of filters used in the query. You might be able to get more accurate results by removing some `WHERE`- or `HAVING`-clauses from your query. Use the [avg_nise](#avg_nise), [count_noise](#count_noise), and [sum_noise](#sum_noise)  functions to get a better idea of how much noise is being added.

### `null` and aggregates of infrequently occurring values

Aircloak will report a value when the number of distinct users sharing the value exceeds a minimum threshold.

For example a query like

<pre style="float:left; background-color:inherit; color:inherit; text-shadow:inherit; padding-top: inherit;">
  SELECT name
  FROM users
  GROUP BY name
</pre>

can safely return even infrequently occuring names.

The threshold for reporting a value, which is low (but safe), does under some circumstances not allow the system to produce anonymized aggregate values.
When this occurs `null` will be returned instead of an aggregate value. In the case of the `COUNT` aggregate the threshold value
is returned instead of `null` to remain compliant with standard SQL where `COUNT` is expected to return a non-null value.

As an example, let's consider a dataset containing 4 users with `name` Alice and an `age` column.
A query attempting to return aggregate properties of the `age` column will likely return a set of `null` values.

<pre style="float:left; background-color:inherit; color:inherit; text-shadow:inherit; padding-top: inherit;">
  SELECT
    name,
    count(*), count_noise(*),
    sum(age), sum_noise(age),
    avg(age), avg_noise(age)
  FROM users
  GROUP BY name
</pre>

Notice how `COUNT` still produces a non-`NULL` value. The reported count is not accurate but signifies an absolute lower
bound.

| name  | count | count_noise | sum  | sum_noise | avg  | avg_noise |
|-------|-------|-------------|------|-----------|------|-----------|
| Alice | 2     | null        | null | null      | null | null      |


## Anonymization functions

### avg_nise

```sql
AVG_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `AVG(...)` expression.

### count_noise

```sql
COUNT_NOISE(*)

COUNT_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `COUNT(...)` expression.

### sum_noise

```sql
SUM_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `SUM(...)` expression.

## Date functions

The functions `year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, and `weekday` are supported. They extract
the named part from a date or time column.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

## Mathematical operators

```sql
1 - 2 + 4 * 3 / 2
-- 5

2 ^ 3
-- 8

7 % 3
-- 1
```

The operators `+`, `-`, `/`, and `*` have their usual meaning of addition, subtraction, division, and
multiplication respectively. The operator `^` denotes exponentiation. The operator `%` denotes the division
remainder.

[Restrictions in usage apply](#math-and-function-application-restrictions).


## Mathematical functions

### abs

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

Computes the absolute value of the given number.

[Restrictions in usage apply](#math-and-function-application-restrictions).


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

[Restrictions in usage apply](#math-and-function-application-restrictions).


### ceil / ceiling

```sql
CEIL(3.22)
-- 4
```

Computes the smallest integer that is greater than or equal to its argument.

[Restrictions in usage apply](#math-and-function-application-restrictions).


### div

```sql
DIV(10, 2)
-- 5

DIV(10, 3)
-- 3
```

Performs integer division on its arguments.

[Restrictions in usage apply](#math-and-function-application-restrictions).


### floor

```sql
FLOOR(3.22)
-- 3
```

Computes the largest integer that is less than or equal to its argument.

[Restrictions in usage apply](#math-and-function-application-restrictions).


### mod

```sql
MOD(10, 3)
-- 1
```

`MOD(a, b)` computes the remainder from `DIV(a, b)`.

[Restrictions in usage apply](#math-and-function-application-restrictions).


### pow

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

`POW(a, b)` computes `a` to the `b`-th power.

[Restrictions in usage apply](#math-and-function-application-restrictions).


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

[Restrictions in usage apply](#math-and-function-application-restrictions).


### sqrt

```sql
SQRT(2)
-- 1.4142135623730951
```

Computes the square root.

[Restrictions in usage apply](#math-and-function-application-restrictions).


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

[Restrictions in usage apply](#math-and-function-application-restrictions).


## String functions

### btrim

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.

[Restrictions in usage apply](#math-and-function-application-restrictions).

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

[Restrictions in usage apply](#math-and-function-application-restrictions).


### length

```sql
LENGTH('some text')
-- 9
```

Computes the number of characters in the string.

[Restrictions in usage apply](#math-and-function-application-restrictions).


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

[Restrictions in usage apply](#math-and-function-application-restrictions).


### right

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.

[Restrictions in usage apply](#math-and-function-application-restrictions).


### rtrim

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

Removes all of the given characters from the end of the string. The default is to remove spaces.

[Restrictions in usage apply](#math-and-function-application-restrictions).


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

[Restrictions in usage apply](#math-and-function-application-restrictions).


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
CAST('3' AS integer)
-- 3

'3'::integer
-- 3

CAST(3, text)
-- '3'

CAST('NOT A NUMBER', integer)
-- NULL
```

You can convert values between different types using a cast expression.

[Restrictions in usage apply](#math-and-function-application-restrictions).

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
