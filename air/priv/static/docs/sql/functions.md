# Functions

## Date/time functions

### Current date/time functions

The functions `current_date`, `current_time`, `current_datetime`, `now` and `current_timestamp` are supported. They
are evaluated during query compilation and replaced with the current date and/or time value.

```sql
current_date()
-- 2016-05-22

-- current_timestamp(), now() and current_datetime() are identical
current_timestamp()
-- 2016-05-22 12:30:21.340

current_time()
-- 12:22:44.220
```

### Date extraction functions

The functions `year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, `weekday`, and `dow` (a synonym for
`weekday`) are supported. They extract the named part from a date or time column.

Functions `weekday` and `dow` return values in interval 1 (Sunday) to 7 (Saturday).
This behavior may change if database defaults are modified.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

### date_trunc

"Rounds" the date or time to the given precision. Supported precision levels
are `year`, `quarter`, `month`, `day`, `hour`, `minute`, and `second`.

```sql
DATE_TRUNC('quarter', date)
-- 2016-05-22 12:30:00.000 -> 2016-04-01 00:00:00.000

DATE_TRUNC('hour', time)
-- 12:22:44.000 -> 12:00:00.000
```

## Working with intervals

When subtracting two date or time columns the result is an interval. The format Aircloak follows when representing
intervals is [ISO-8601](https://en.wikipedia.org/wiki/ISO_8601#Durations).

```sql
cast('2017-01-02' as date) - cast('2017-01-01' as date)
-- P1D

cast('12:33:44' as time) - cast('11:22:33' as time)
-- PT1H11M11S

cast('2017-02-03 11:22:33' as timestamp) - cast('2016-01-02 12:33:44' as timestamp)
-- P1Y1M2DT22H48M49S

-- Intervals do not have a sign
cast('12:00:00' as time) - cast('13:00:00' as time)
-- PT1H
cast('13:00:00' as time) - cast('12:00:00' as time)
-- PT1H
```

Similarly, an interval can be added or subtracted from a date or time column.

```sql
cast('13:00:00' as time) + interval 'PT1H2M3S'
-- 14:02:03

cast('2015-07-06 12:00:00' as timestamp) - interval 'P1Y1M1DT1H1M1S'
-- 2014-06-05 10:58:59

-- Note that months in intervals will always have 30 days
cast('2015-06-06' as date) - interval 'P1M'
-- 2015-05-07 00:00:00
cast('2015-07-06' as date) - interval 'P1M'
-- 2015-06-06 00:00:00

-- Similarly years will always have 365 days
cast('2015-06-06' as date) + interval 'P1Y'
-- 2016-06-05 00:00:00
cast('2016-06-06' as date) + interval 'P1Y'
-- 2017-06-06 00:00:00
```

Intervals can be multiplied or divided by numbers to yield bigger or smaller intervals.

```sql
2 * interval 'P1Y'
-- P2Y

0.5 * interval 'P1M'
-- P15D

interval 'PT1H' / 2
-- PT30M
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)

## Mathematical operators

The operators `+`, `-`, `/`, and `*` have their usual meaning of addition, subtraction, division, and
multiplication respectively.

```sql
1 - 2 + 4 * 3 / 2
-- 5
```

The operator `^` denotes exponentiation.

```
2 ^ 3
-- 8

-- Note that ^ is left-associative

2 ^ 3 ^ 4
-- 4 096

2 ^ (3 ^ 4)
-- 2 417 851 639 229 258 300 000 000
```

The operator `%` computes the division remainder:

```
33 % 10
-- 3
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)

## Mathematical functions

### abs

Computes the absolute value of the given number.

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### bucket

Rounds the input to multiples of N, where N is provided in the `BY` argument. It also accepts an `ALIGN` argument to
specify if the rounding should occur down (`ALIGN LOWER` - this is the default), up (`ALIGN UPPER`), or if an average
between the two should be returned (`ALIGN MIDDLE`).

```sql
BUCKET(180 BY 50)
-- 150

BUCKET(150 BY 50)
-- 150

BUCKET(200 BY 50)
-- 200

BUCKET(180 BY 100)
-- 100

BUCKET(180 BY 50 ALIGN LOWER)
-- 150

BUCKET(180 BY 50 ALIGN UPPER)
-- 200

BUCKET(180 BY 50 ALIGN MIDDLE)
-- 175
```

This function is useful to prepare buckets/bins for a histogram, for example in a query like the following:

```sql
SELECT BUCKET(price BY 5), COUNT(*)
FROM purchases
GROUP BY 1
-- bucket count
-- 0      10     - all purchases priced below 5
-- 5      10     - purchases priced at or above 5 and below 10
-- 10     20     - purchases priced at or above 10 and below 15
-- etc.
```

The function can also help if the column you want to group by has many unique values and many of the buckets get
anonymized away.  For example if you have a column containing the length of a call in seconds:

```sql
SELECT call_duration, COUNT(*)
FROM calls
GROUP BY 1
-- call_duration count
-- *             100

SELECT BUCKET(call_duration BY 5), COUNT(*)
FROM calls
GROUP BY 1
-- bucket count
-- *      20
-- 0      10
-- 5      10
-- etc.
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### ceil / ceiling

Computes the smallest integer that is greater than or equal to its argument.

```sql
CEIL(3.22)
-- 4
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### floor

Computes the largest integer that is less than or equal to its argument.

```sql
FLOOR(3.22)
-- 3
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### pow

`POW(a, b)` computes `a` to the `b`-th power. Returns NULL if `a` is negative.

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### round

Rounds the given floating-point value to the nearest integer. An optional second argument signifies the precision.
Halves are rounded away from zero (towards the larger absolute value).

```sql
ROUND(3.22)
-- 3

ROUND(3.99)
-- 4

ROUND(3.22, 1)
-- 3.2
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


### sqrt

Computes the square root.

```sql
SQRT(2)
-- 1.4142135623730951
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)


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

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)

## String operators

### ||

Joins two or more strings into one. It is internally translated to the `concat` function.

```sql
'a' || 'b' || 'c'
-- 'abc'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### LIKE / ILIKE

These operators match a text expression against a pattern. `ILIKE` is the case-insensitive version of `LIKE`.
Syntax: `text_expression [NOT] LIKE | ILIKE string_pattern [ESCAPE escape_string]`.

If the pattern does not contain any percent signs or underscores, then the pattern only represents the string itself.
In that case, `LIKE` acts like the equals operator and `ILIKE` acts like a case-insensitive equals operator.
An underscore (`_`) in a pattern matches any single character.
A percent sign (`%`) matches any string of zero or more characters.

A pattern match needs to cover the entire string. To match a sequence anywhere within a string, the pattern must
therefore start and end with a percent sign.

To match a literal underscore or percent sign without matching other characters, the respective character in the
pattern must be preceded by the escape character. No escape character is set by default. To match the escape
character itself, write two escape characters.

```sql
'abCD' LIKE 'ab%'
-- TRUE

'abCD' NOT LIKE 'ab__'
-- FALSE

email ILIKE 'a\_b@%.com' ESCAPE '\'
-- BOOLEAN
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

## String functions

### btrim

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

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

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### left

`LEFT(string, n)` takes n characters from the beginning of the string. If n is negative takes all but the last |n| characters.

```sql
LEFT('some text', 4)
-- 'some'

LEFT('some text', -2)
-- 'some te'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### length

Computes the number of characters in the string.

```sql
LENGTH('some text')
-- 9
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

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

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### right

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### rtrim

Removes all of the given characters from the end of the string. The default is to remove spaces.

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

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

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

### trim

Removes all of the given characters from the beginning and/or end of the string.
The default is to remove spaces from both ends.

```sql
TRIM(' some text ')
-- 'some text'

TRIM(LEADING ' some text ')
-- 'some text '

TRIM(TRAILING ' tx' FROM ' some text ')
-- ' some te'

TRIM(' osxt' ' some text ')
-- 'me te'

TRIM(BOTH FROM ' some text ')
-- 'some text'
```

[Restrictions in usage apply](/sql/restrictions.md#text-operations)

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
CAST('3' AS integer)
-- 3

'3'::integer
-- 3

CAST(3, text)
-- '3'

CAST('NOT A NUMBER', integer)
-- NULL
```

[Restrictions in usage apply](/sql/restrictions.md#math-and-function-application-restrictions)

Types can be converted according to the following tables:

| from\to  | text  | integer | real  | boolean |
| :------- | :---: | :-----: | :---: | :-----: |
| text     |   ✓   |    ✓    |   ✓   |    ✓    |
| integer  |   ✓   |    ✓    |   ✓   |    ✓    |
| real     |   ✓   |    ✓    |   ✓   |    ✓    |
| boolean  |   ✓   |    ✓    |   ✓   |    ✓    |
| date     |   ✓   |         |       |         |
| time     |   ✓   |         |       |         |
| datetime |   ✓   |         |       |         |
| interval |   ✓   |         |       |         |

| from\to  | date  | time  | datetime | interval |
| :------- | :---: | :---: | :------: | :------: |
| text     |   ✓   |   ✓   |    ✓     |    ✓     |
| integer  |       |       |          |          |
| real     |       |       |          |          |
| boolean  |       |       |          |          |
| date     |   ✓   |       |          |          |
| time     |       |   ✓   |          |          |
| datetime |   ✓   |   ✓   |    ✓     |          |
| interval |       |       |          |    ✓     |

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

## Aggregation functions

### Note about noise

Unlike in regular database systems, the results of aggregation functions will usually not be reported precisely.
Instead, a small amount of noise will be added or subtracted from the real value to preserve anonymity. See [the section
about *_noise functions](#noise) for more on how to get a measure of how much noise is added.

### avg

Computes the average of the given expression.

```sql
SELECT avg(age) FROM people

          avg
  -------------------
   29.44782928323982

SELECT lastname, avg(age) FROM people GROUP BY 1

   lastname |        avg
  ----------+--------------------
   ABBOTT   | 28.930111858960856
   ACEVEDO  | 29.933255031072672
   ...      | ...
```

Note that the computed average is anonymized by introducing a certain amount of noise. See [Note about
noise](#note-about-noise) for more.

### count

Computes the number of rows for which the given expression is non-NULL. Use `*` as an argument to count all rows.

```sql
SELECT count(age) FROM people

   count
  -------
   10000

SELECT lastname, count(age) FROM people GROUP BY 1

   lastname | count
  ----------+-------
   ABBOTT   |    10
   ACEVEDO  |    12
   ...      |   ...

```

Note that in order to preserve anonymity Insights Cloak will never report "groups" of just one user and there will be
noise added to the result. Because of this, when you see a count of 2, it should be treated as a placeholder value for
"a small number, not lower than 2". See [Note about noise](#note-about-noise) for more.

### max

Finds the maximum value of the given expression.

```sql
SELECT max(age) FROM people

   max
  -----
    43

SELECT lastname, max(age) FROM people GROUP BY 1

   lastname | max
  ----------+-----
   ABBOTT   |  30
   ACEVEDO  |  32
   ...      | ...
```

Note that the computed max value is anonymized - it requires a number of users to share this value, so in many cases
the true value will be larger. Furthermore, Insights Cloak's anonymizing `max` function doesn't work on textual values:

```sql
SELECT max(lastname) FROM people

  ERROR:  Aggregator `max` is not allowed over arguments of type `text` in anonymizing contexts.
  For more information see the "Text operations" subsection of the "Restrictions" section in the user guides.
```

However, you can still use `max` to postprocess textual results of an anonymizing subquery:

```sql
SELECT max(lastname) FROM (SELECT lastname FROM people GROUP BY 1) x

    max
  --------
   ZUNIGA
```

### min

Finds the minimum value of the given expression.

```sql
SELECT min(age) FROM people

   min
  -----
    16

SELECT lastname, min(age) FROM people GROUP BY 1

   lastname | min
  ----------+-----
   ABBOTT   |  26
   ACEVEDO  |  28
   ...      | ...
```

Note that the computed min value is anonymized - it requires a number of users to share this value, so in many cases
the true value will be smaller. Furthermore, Insights Cloak's anonymizing `min` function doesn't work on textual values:

```sql
SELECT min(lastname) FROM people

  ERROR:  Aggregator `min` is not allowed over arguments of type `text` in anonymizing contexts.
  For more information see the "Text operations" subsection of the "Restrictions" section in the user guides.
```

However, you can still use `min` to postprocess textual results of an anonymizing subquery:

```sql
SELECT min(lastname) FROM (SELECT lastname FROM people GROUP BY 1) x

    min
  --------
   ABBOTT
```

### stddev

Computes the sample standard deviation of the given numerical expression.

```sql
SELECT stddev(age) FROM people

        stddev
  -------------------
   4.032164086149982

SELECT lastname, stddev(age) FROM people GROUP BY 1

   lastname |       stddev
  ----------+--------------------
   ABBOTT   | 7.2835052504058195
   ACEVEDO  |  1.587458159104735
   ...      |                ...
```

Note that the computed standard deviation is anonymized by introducing a certain amount of noise. See [Note about
noise](#note-about-noise) for more.

### sum

Computes the sum of the given numerical expression.

```sql
SELECT sum(points) FROM games

     sum
  ---------
   6390144

SELECT date, sum(points) FROM games GROUP BY 1

      date    | sum
  ------------+------
   2013-01-01 | 5510
   2013-01-02 | 6761
   ...        |  ...
```

Note that the computed sum is anonymized by introducing a certain amount of noise. See [Note about
noise](#note-about-noise) for more.

### variance

Computes the sample variance of the given numerical expression.

```sql
SELECT variance(age) FROM people

       variance
  -------------------
   16.25834721763772

SELECT lastname, variance(age) FROM people GROUP BY 1

   lastname |      variance
  ----------+--------------------
   ABBOTT   |  53.04944873268914
   ACEVEDO  | 2.5200234069081944
   ...      |                ...
```

Note that the computed variance is anonymized by introducing a certain amount of noise. See [Note about
noise](#note-about-noise) for more.

### *_noise

You can get a sense of how much noise is being added to an `avg`, `count`, `stddev`, `sum`, or `variance` expression by
using an analogous `*_noise` expression. The value returned is the standard deviation of the noise added according to
what's described in the [section about noise](/sql/query-results.md#zero-mean-noise).

```sql
SELECT count(*), count_noise(*), avg(age), avg_noise(age) FROM people

   count | count_noise |        avg        | avg_noise
  -------+-------------+-------------------+-----------
   10000 |         1.0 | 29.44782928323982 |    0.0029

SELECT lastname, count(*), count_noise(*), avg(age), avg_noise(age) FROM players GROUP BY 1

   lastname | count |    count_noise     |        avg         | avg_noise
  ----------+-------+--------------------+--------------------+-----------
   ABBOTT   |    10 | 1.4000000000000001 | 28.930111858960856 |       4.2
   ACEVEDO  |    12 | 1.4000000000000001 | 29.933255031072672 |       3.5
   ...      |   ... |                ... |                ... |       ...
```

Note that the noise added depends on the expression inside the aggregation function used, so you have to provide the
exact same expression in the `*_noise` function to get an accurate value:

```sql
SELECT avg(age), avg_noise(age),
  avg(age * age) AS square, avg_noise(age * age) AS square_noise
FROM people

          avg        | avg_noise |      square       | square_noise
  -------------------+-----------+-------------------+--------------
   29.44782928323982 |    0.0029 | 883.4329967124744 |         0.09
```

## Special functions

### grouping_id

Returns an integer bitmask for the columns used in the current grouping set.
Can only be used in the `SELECT`, `HAVING` and `ORDER BY` clauses when the `GROUP BY` clause is specified.

Each `grouping_id` argument must be an element of the `GROUP BY` list. Bits are assigned with the rightmost argument
being the least-significant bit; each bit is 0 if the corresponding expression is included in the grouping criteria of
the grouping set generating the result row, and 1 if it is not.

```sql
SELECT
   alive,
   bucket(age by 10) as age,
   count(*),
   grouping_id(alive, bucket(age by 10))
FROM demographics
GROUP BY CUBE (1, 2)

   alive |  age  | count | grouping_id
   ------+-------+-------+------------
   false |   *   |  10   |      0
   false |  20   |   7   |      0
   true  |  10   |   3   |      0
   true  |  20   |   2   |      0
   true  |  30   |   4   |      0
   false |       |  14   |      1
   true  |       |  13   |      1
         |   *   |   2   |      2
         |  10   |   7   |      2
         |  20   |  10   |      2
         |  30   |   6   |      2
         |       |  31   |      3
```
