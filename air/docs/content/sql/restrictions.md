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
