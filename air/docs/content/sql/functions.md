# Functions

## Date/time functions

### Date extraction functions

The functions `year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, `weekday`, and `dow` (a synonym for
`weekday`) are supported. They extract the named part from a date or time column.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

### date_trunc

"Rounds" the date or time to the given precision. Supported precision levels
are `year`, `quarter`, `month`, `day`, `hour`, `minute`, and `second`.

```sql
DATE_TRUNC('quarter', date)
-- 2016-05-22T12:30:00.000000 -> 2016-04-01T00:00:00.000000

DATE_TRUNC('hour', time)
-- 12:22:44.000000 -> 12:00:00.000000
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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

## Mathematical operators

The operators `+`, `-`, `/`, and `*` have their usual meaning of addition, subtraction, division, and
multiplication respectively. The operator `^` denotes exponentiation.

```sql
1 - 2 + 4 * 3 / 2
-- 5

2 ^ 3
-- 8
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

## Mathematical functions

### abs

Computes the absolute value of the given number.

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### bucket

Rounds the input to the nearest N, where N is provided in the `BY` argument. It also accepts an `ALIGN` argument to
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
SELECT BUCKET(price BY 5 ALIGN UPPER), COUNT(*)
FROM purchases
GROUP BY 1
-- bucket count
-- 0      10     - all purchases priced below 5
-- 5      10     - purchases priced at or above 5 and below 10
-- 10     20     - purchases priced at or above 10 and below 15
-- etc.
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### ceil / ceiling

Computes the smallest integer that is greater than or equal to its argument.

```sql
CEIL(3.22)
-- 4
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### floor

Computes the largest integer that is less than or equal to its argument.

```sql
FLOOR(3.22)
-- 3
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### pow

`POW(a, b)` computes `a` to the `b`-th power.

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### sqrt

Computes the square root.

```sql
SQRT(2)
-- 1.4142135623730951
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


## String functions

### btrim

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

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

### extract_words

Splits a string into words, each becoming an individual row.

```sql
EXTRACT_WORDS('Some Text')
-- 'Some'
-- 'Text'
```

This function is not allowed in subqueries.

Keep in mind that this function might affect your analysis in unexpected ways.
When a column value is split into multiple values, the whole row is replecated.
This will affect the behaviour of other aggregate functions!

For example, consider the following row coming from the database:

| user-id | price | description |
|---------|-------|-------------|
| 1 | 10.00 | purchase of a book |
| 2 | 15.00 | book of the year |

If used in conjunction with `extract_words(description)`, the input data will be
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

### hex

Transforms all characters in the given string into hexadecimal.
This is useful for extracting strings containing non-text characters.

```sql
HEX('air')
-- '616964'
```

### left

`LEFT(string, n)` takes n characters from the beginning of the string. If n is negative takes all but the last |n| characters.

```sql
LEFT('some text', 4)
-- 'some'

LEFT('some text', -2)
-- 'some te'
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

### length

Computes the number of characters in the string.

```sql
LENGTH('some text')
-- 9
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

### right

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

### rtrim

Removes all of the given characters from the end of the string. The default is to remove spaces.

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

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
-- 'some text '
```

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

Types can be converted according to the following tables:

| from\to   | text | integer | real | boolean |
|----------:|:----:|:-------:|:----:|:-------:|
| text      | ✓    | ✓       | ✓    | ✓       |
| integer   | ✓    | ✓       | ✓    | ✓       |
| real      | ✓    | ✓       | ✓    | ✓       |
| boolean   | ✓    | ✓       | ✓    | ✓       |
| date      | ✓    |         |      |         |
| time      | ✓    |         |      |         |
| datetime  | ✓    |         |      |         |
| interval  | ✓    |         |      |         |

| from\to   | date | time | datetime  | interval |
|----------:|:----:|:----:|:---------:|:--------:|
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
