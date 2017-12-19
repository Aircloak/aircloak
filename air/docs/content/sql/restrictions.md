# Restrictions

The Aircloak system imposes a set of restrictions on the query language over the ANSI SQL standard.
These restrictions are outlined below.


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

- `OUTER` is automatically implied when you use `LEFT`, `RIGHT` joins. Writing `LEFT OUTER JOIN` is therefore equivalent to writing `LEFT JOIN`
- `INNER` is automatically implied when you use `JOIN` without any other qualifiers. Writing `t1 JOIN t2` is therefore the same as writing `t1 INNER JOIN t2`


## Subquery restrictions

A subquery expression with an aggregate must always select the user-id column as well. For example, assuming there exists a table `t1` with a user-id column called `uid`:

- __Valid__: `SELECT name FROM (SELECT name FROM t1) sq`
- __Valid__: `SELECT name FROM (SELECT uid, count(*) FROM t1 GROUP BY uid) sq`
- __Invalid__: `SELECT name FROM (SELECT count(*) FROM t1) sq`

When using `LIMIT` and `OFFSET` in a subquery:

- `LIMIT` is required if `OFFSET` is specified
- `LIMIT` will be adjusted to the closest number in the sequence `[10, 20, 50, 100, 200, 500, 1000, ...]` (i.e. `10e^n`, `20e^n`, `50e^n`
  for any natural number `n` larger than 0). For example: 1 or 14 become 10, etc
- `OFFSET` will automatically be adjusted to the nearest multiple of `LIMIT`. For example an `OFFSET` of 240 will be
  adjusted to 200 given a `LIMIT` of 100


## Top-level HAVING clause

Any conditions specified in the `HAVING` clause of the top-level query (_not_ a subquery) are "safe" in the sense that
they will only ever be applied to data that has already been aggregated and anonymized. The clause will merely affect
which of the anonymized data to display, not how that data is obtained.  Because of this, many of the restrictions
described in the following sections don't apply to the top-level `HAVING` clause.


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
    abs(div(age, 2)) + height as value1
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


## Ranges

Whenever an inequality (`>`, `>=`, `<`, or `<=`) is used in a `WHERE`-, `JOIN`- or `HAVING`-clause that clause actually needs to contain two
inequalities. These should form a range on a single column or expression. That is, one `>` or `>=` inequality and one `<` or `<=`
inequality, limiting the column/expression from bottom and top.

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

Note that a condition using the `BETWEEN` operator automatically forms a range:

```sql
-- These two queries are equivalent:
SELECT COUNT(*) FROM table WHERE column BETWEEN 10 AND 20
SELECT COUNT(*) FROM table WHERE column >= 10 AND column < 20
```


## Range alignment

The system will adjust ranges provided in queries. The adjustment will "snap" the range to a fixed, predefined grid. It will always
make sure that the specified range is included in the adjusted range. The range will also be modified to be closed on the left (`>=`)
and open on the right (`<`).

If any such modifications take place an appropriate notice will be displayed in the web interface. When using the API the notice will
be included under the `info` key of the result. The notice will _not_ appear when using the PostgreSQL interface.

The grids available depend on the type of the column that is being limited by the range. For numerical columns the grid sizes are
`[..., 0.1, 0.2, 0.5, 1, 2, 5, 10, ...]`. For date/time columns they are `[1, 2, 5, ...]` years, `[1, 2, 6, 12]` months, `[1, 2, 5, ...]` days,
`[1, 2, 6, 12, 24]` hours, `[1, 2, 5, 15, 30, 60]` minutes, and `[1, 2, 5, 15, 30, 60]` seconds.

To arrive at the final range the system finds the smallest grid size that will contain the given range. Then it shifts the lower end of the
range to be a multiple of half of the grid size. The upper end is just the lower end plus the grid size. In some cases halving the grid size
is not allowed and the lower end needs to be a multiple of the whole grid size instead - for example it is not allowed to halve days in
case the underlying data type is `date` and cannot represent such halving. See the examples below for details.

For best results design your queries so that they take this adjustment into account and mostly use ranges that are already adjusted.

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


## Implicit ranges

Some functions can be used to almost the same effect as a pair of inequalities. For example the following two queries
are roughly equivalent:

```sql
SELECT COUNT(*) FROM table WHERE round(number) = 10
SELECT COUNT(*) FROM table WHERE number >= 9.5 AND number < 10.5
```

Because of this, usage of such functions must be restricted in a similar way to inequalities and the `BETWEEN` operator.
The restrictions disallow the usage of any functions or mathematical operations before or after applying an implicit
range function. The only operations that can be applied are a single `CAST` and any number of aggregators (`MIN`, `MAX`,
`COUNT`, `SUM`, `AVG`, `STDDEV`). The restrictions apply when an implict range function is used in a `WHERE` or `JOIN`
clause, selected in the top-level `SELECT` clause or used in a non-top-level `HAVING` clause.

```sql
-- Correct - no other function used
SELECT COUNT(*) FROM table WHERE round(number) = 10

-- Correct - an aggregate is used
SELECT COUNT(*) FROM table GROUP BY category WHERE round(max(number)) = 10

-- Incorrect - another operation (+) is applied in the same expression as round
SELECT COUNT(*) FROM table WHERE round(number + 1) = 10

-- Correct - used in the top-level HAVING, so restrictions don't apply
SELECT COUNT(*) FROM table GROUP BY category HAVING round(max(number) + 1) = 10

-- Incorrect - used in a non-top-level HAVING
SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY category HAVING round(max(number) + 1) = 10) x

-- Incorrect - another operation is used in top-level SELECT
SELECT round(abs(number)) FROM table
```

The following functions are treated as implicit range functions: `round`, `trunc`, `date_trunc`, and all date extraction
functions (`year`, `month`, `quarter`, `day`, `weekday`, `hour`, `minute`, `second`).


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
manipulation functions are the same as ones described for [implicit ranges](#implicit-ranges). In addition a result of
text manipulation can only be compared to an untransformed column or a constant.

```sql
-- Correct
SELECT COUNT(*) FROM table WHERE LEFT(name, 1) = 'A'

-- Incorrect - the results of a text operation are compared to a complex expression
SELECT COUNT(*) FROM table WHERE LEFT(name, 1) = UPPER(RIGHT(name, 1))
```

The following functions are treated as text manipulation functions: `left`, `right`, `rtrim`, `ltrim`, `trim`, and
`substring`.


## IN, NOT IN, NOT LIKE, and <>

Any conditions using `IN`, `NOT LIKE`, `NOT ILIKE`, or `<>` are subject to additional restrictions. Note that `NOT IN`
is treated just as `<>`, because there is always an equivalent query using `<>` for every `NOT IN` query:

```sql
-- These queries are equivalent
SELECT COUNT(*) FROM table WHERE number NOT IN (1, 2, 3)
SELECT COUNT(*) FROM table WHERE number <> 1 AND number <> 2 AND number <> 3
```

Conditions using `IN` or `<>` cannot include any functions nor mathematical operations except the following: `lower`,
`upper`, `substring`, `trim`, `ltrim`, `rtrim`, `btrim`, `extract_words`, and all aggregators (`MIN`, `MAX`, `COUNT`,
`SUM`, `AVG`, `STDDEV`). Conditions using `NOT LIKE` or `NOT ILIKE` cannot contain any functions except for aggregators.
A single `CAST` is allowed. Furthermore, one of the expressions being compared must be a constant. In the case of `IN`
all items on the right-hand side of the `IN` operator must be constants. An exception to this is comparing two columns
with `<>`, but in that case no functions can be used at all. The top-level `HAVING` clause is exempt from  all these
restrictions.

```sql
-- Correct
SELECT COUNT(*) FROM table WHERE lower(name) <> 'alice'

-- Incorrect - a disallowed operation was used
SELECT COUNT(*) FROM table WHERE left(name, 1) <> 'a'

-- Incorrect - a comparison between two complex expressions
SELECT COUNT(*) FROM table WHERE lower(name) <> lower(surname)

-- Correct - top-level HAVING is exempt from restrictions
SELECT COUNT(*) FROM table GROUP BY name HAVING left(name) <> 'a'

-- Correct - comparing two database columns
SELECT COUNT(*) FROM table WHERE name <> surname
```
