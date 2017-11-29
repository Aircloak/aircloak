# Functions

## Date/time functions

### Date extraction functions

The functions `year`, `quarter`, `month`, `day`, `hour`, `minute`, `second`, and `weekday` are supported. They extract
the named part from a date or time column.

```sql
SELECT YEAR(date_column), MONTH(date_column), DAY(date_column) FROM table;

SELECT EXTRACT(year FROM date_column) FROM table;
```

### date_trunc

```sql
DATE_TRUNC('quarter', date)
-- 2016-05-22T12:30:00.000000 -> 2016-04-01T00:00:00.000000

DATE_TRUNC('hour', time)
-- 12:22:44.000000 -> 12:00:00.000000
```

"Rounds" the date or time to the given precision. Supported precision levels
are `year`, `quarter`, `month`, `day`, `hour`, `minute`, and `second`.

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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)

## Mathematical functions

### abs

```sql
ABS(3)
-- 3

ABS(-3)
-- 3
```

Computes the absolute value of the given number.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### ceil / ceiling

```sql
CEIL(3.22)
-- 4
```

Computes the smallest integer that is greater than or equal to its argument.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### div

```sql
DIV(10, 2)
-- 5

DIV(10, 3)
-- 3
```

Performs integer division on its arguments.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### floor

```sql
FLOOR(3.22)
-- 3
```

Computes the largest integer that is less than or equal to its argument.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### mod

```sql
MOD(10, 3)
-- 1
```

`MOD(a, b)` computes the remainder from `DIV(a, b)`.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### pow

```sql
POW(2, 3)
-- 8

POW(2, 3.5)
-- 11.313708498984761
```

`POW(a, b)` computes `a` to the `b`-th power.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### sqrt

```sql
SQRT(2)
-- 1.4142135623730951
```

Computes the square root.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


## String functions

### btrim

```sql
BTRIM(' some text ')
-- 'some text'

BTRIM('xyzsome textzyx', 'xyz')
-- 'some text'
```

Removes all of the given characters from the beginning and end of the string. The default is to remove spaces.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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


### extract_words

```sql
EXTRACT_WORDS('Some Text')
-- 'Some'
-- 'Text'
```

Splits a string into words, each becoming an individual row.

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

```sql
HEX('air')
-- '616964'
```

Transforms all characters in the given string into hexadecimal.
This is useful for extracting strings containing non-text characters.


### left

```sql
LEFT('some text', 4)
-- 'some'

LEFT('some text', -2)
-- 'some te'
```

`LEFT(string, n)` takes n characters from the beginning of the string. If n is negative takes all but the last |n| characters.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### length

```sql
LENGTH('some text')
-- 9
```

Computes the number of characters in the string.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### right

```sql
RIGHT('some text', 4)
-- 'text'

RIGHT('some text', -2)
-- 'me text'
```

`RIGHT(string, n)` takes n characters from the end of the string. If n is negative takes all but the first |n| characters.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### rtrim

```sql
RTRIM(' some text ')
-- ' some text'

RTRIM('xyzsome textzyx', 'xyz')
-- 'xyzsome text'
```

Removes all of the given characters from the end of the string. The default is to remove spaces.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


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

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### trim

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

Removes all of the given characters from the beginning and/or end of the string.
The default is to remove spaces from both ends.

[Restrictions in usage apply](restrictions.html#math-and-function-application-restrictions)


### upper

```sql
UPPER('Some Text')
-- 'SOME TEXT'

UCASE('Some Text')
-- 'SOME TEXT'
```

Transforms all characters in the given string into uppercase.


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
