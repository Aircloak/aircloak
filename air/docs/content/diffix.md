
<!----
This document contains links to associated issues. Each link has the label [ghiXXXX](url), where XXXX is the issue number. These links should always be on a separate line. This way, we can easily filter them out when we make public versions of this document.
---->

# Specification for Diffix Cedar

# Table Of Contents

- [Overview](#overview)
  - [Key concepts](#key-concepts)
  - [Deployment](#deployment)
  - [Main Pipeline](#main-pipeline)
  - [Database](#database)
- [Initialization of internal state](#initialization-of-internal-state)
    - [Shadow table](#shadow-table)
    - [Isolating column label](#isolating-column-label)
    - [Safe math functions](#safe-math-functions)
    - [Per column min and max values](#per-column-min-and-max-values)
- [Handle incoming SQL](#handle-incoming-sql)
  - [Supported SQL](#supported-sql)
  - [Rejecting queries](#rejecting-queries)
    - [Inequalities](#inequalities)
    - [Clear conditions (IN, range, negative conditions)](#clear-conditions-in-range-negative-conditions)
    - [Too much math](#too-much-math)
      - [In general](#in-general)
      - [With isolating columns](#with-isolating-columns)
    - [String functions](#string-functions)
    - [LIKE and NOT LIKE](#like-and-not-like)
    - [Other limitations of isolating columns](#other-limitations-of-isolating-columns)
    - [Limitations due to shadow table](#limitations-due-to-shadow-table)
    - [Conditions with two columns](#conditions-with-two-columns)
  - [Modifying queries](#modifying-queries)
    - [Snapped Ranges](#snapped-ranges)
- [Aggregation functions sum, min, max, count](#aggregation-functions-sum-min-max-count)
  - [Generate DB query](#generate-db-query)
    - [Gather bucket statistics](#gather-bucket-statistics)
    - [Gather seed materials](#gather-seed-materials)
    - [Determine if safe math functions needed](#determine-if-safe-math-functions-needed)
    - [Add protection against JOIN timing attack](#add-protection-against-join-timing-attack)
    - [Add protection against divide-by-zero attacks](#add-protection-against-divide-by-zero-attacks)
    - [Add protection against square root of negative numbers](#add-protection-against-square-root-of-negative-numbers)
  - [Handle DB answer](#handle-db-answer)
    - [Noise Layers](#noise-layers)
    - [Determine seeds](#determine-seeds)
    - [Low count suppression](#low-count-suppression)
    - [Value flattening and noise addition](#value-flattening-and-noise-addition)
      - [Operation of sum](#operation-of-sum)
      - [Operation of avg](#operation-of-avg)
      - [Operation of min and max](#operation-of-min-and-max)
    - [Reporting suppression](#reporting-suppression)
      - [Bucket merging](#bucket-merging)
- [Aggregation function count distinct](#aggregation-function-count-distinct)
- [Aggregation function stddev](#aggregation-function-stddev)
- [Classes of attacks](#classes-of-attacks)
  - [Attribute value inspection attacks](#attribute-value-inspection-attacks)
  - [Suppression signal attacks](#suppression-signal-attacks)
  - [Noise averaging attacks](#noise-averaging-attacks)
    - [Naive averaging](#naive-averaging)
    - [Different syntax but same semantics, with floating](#different-syntax-but-same-semantics-with-floating)
    - [Different syntax but same semantics, without floating](#different-syntax-but-same-semantics-without-floating)
    - [Split averaging attack](#split-averaging-attack)
    - [Linear program reconstruction](#linear-program-reconstruction)
  - [Difference attacks](#difference-attacks)
    - [First derivative difference attack](#first-derivative-difference-attack)
    - [Difference attack with counting NULL](#difference-attack-with-counting-null)
    - [Noise exploitation attacks](#noise-exploitation-attacks)
      - [Through extreme user contribution](#through-extreme-user-contribution)
      - [Through chaff conditions](#through-chaff-conditions)
    - [Range creep with averaging](#range-creep-with-averaging)
  - [SQL backdoor attacks](#sql-backdoor-attacks)
  - [Side Channel attacks](#side-channel-attacks)
    - [Error generation attacks](#error-generation-attacks)
      - [Divide by zero](#divide-by-zero)
      - [Overflow](#overflow)
      - [Square root of a negative number](#square-root-of-a-negative-number)
    - [Timing attacks](#timing-attacks)
      - [JOIN timing attack](#join-timing-attack)
# Overview

## Key concepts

Anonymization in the cloak has two main aspects, **SQL limiting** and **answer perturbation**. The goal is to allow as much SQL as possible, especially commonly used SQL, and to minimize answer perturbation while maintaining strong anonymity. Strong anonymity is achieved when an attacker's guess as to the attributes of individual users either has low confidence with high probability, or high confidence with low probability.

In order to safely allow as much SQL as possible, the cloak limits SQL in a fine-grained fashion. While some entire SQL features are prevented (i.e. `CASE` statements or window functions), more often an SQL feature is allowed but limited in terms of what other features it may be used in combination with, how frequently it may be used, or even what constant values it may be used with.

Perturbation takes the form of suppressing answers and distorting answers, both through adjusting extreme column values (flattening) and adding noise. Key mechanisms include:

- **Answer suppression:** Answers that pertain to too few distinct users are suppressed.
- **Sticky layered noise:** The cloak adds noise to answers taken from a Gaussian distribution. The noise is sticky in that identical query conditions generate identical noise. The noise is layered in that each query condition contributes a separate noise value (which are summed together to produce the final noise value).
- **Extreme value flattening:** When a few individual users contribute an extreme amount to an answer (relative to other users), then the values contributed by those users are reduced (or increased if negative) to be comparable with values contributed by other users.

The cloak can report how much answer suppression has taken place and how much noise was added to an answer. The cloak cannot, however, report how much extreme value flattening has taken place.

## Deployment

The cloak operates the anonymization function, and is deployed between the un-anonymized database and an analyst (or application). An SQL interface is exposed to the analyst. Though conveyed as such in the illustration below, the interface between cloak and database does not need to be SQL.

```
    +----------+   SQL'   +-------+   SQL
    |          |<---------|       |<----------
    | Database |          | Cloak |            Analyst
    |          |--------->|       |---------->
    +----------+   Data   +-------+  Answer
                                    (buckets)
```

If the SQL query requests an aggregate (i.e. `SELECT age, count(*) FROM table GROUP BY age`), then each row returned in the answer is refered to as a *bucket* in this document. In this example, the count for each age would be a separate bucket.

## Main Pipeline

The cloak operates query-by-query: it accepts SQL queries from the analyst (or application), parses the SQL and checks to ensure that it is allowed, potentially modifies the SQL (i.e. SQL' in the figure above), requests the appropriate data from the database, determines if answers need to be suppressed, and adds noise to unsuppressed answers before returning them to the analyst.

The cloak also maintains some internal state about data in the database. This state is used to determine when certain limitations are necessary. This state is produced by querying the data, and occurs at cloak initialization as well as periodically. 

__Prior to query:__
- [Initialization of internal state](#initialization-of-internal-state) (SQL limiting)

__At query time:__
- [Handle incoming SQL](#handle-incoming-sql) (SQL limiting)
- [Generate DB query](#generate-db-query) (Answer perturbation prep)
- [Handle DB answer](#handle-db-answer) (Answer perturbation)

## Database

The database consists of tables. Tables have *columns* and *rows*. Tables may be *personal* or *non-personal*. Personal tables contain user data that must be anonymized, whereas non-personal tables contain other data. Whether a table is personal or non-personal is determined by configuration.

One column in each personal table must be configured as the UID column. This identifies the entity that must be protected (typically but not necessarily a natural person).

# Initialization of internal state

The cloak establishes cached internal state that it uses to determine which SQL operations are allows on which columns. They are:

### Shadow table

Holds a list of the X values that occur most frequently in the column (X=200), so long as the value has at least 10 distinct users. Values in this list may be used in negands (negative AND conditions) and posors (positive OR conditions) without limitations.
[ghi2486](https://github.com/Aircloak/aircloak/issues/2486)
[ghi2972](https://github.com/Aircloak/aircloak/issues/2972)

This table is refreshed every 30 days.
[ghi3357](https://github.com/Aircloak/aircloak/issues/3357)

### Isolating column label

The cloak labels columns as isolating if 80% or more of the values in the column are associated with only a single user.
[ghi2738](https://github.com/Aircloak/aircloak/issues/2738)
[ghi3396](https://github.com/Aircloak/aircloak/issues/3396)

This table is refreshed every 60 days.
[ghi3357](https://github.com/Aircloak/aircloak/issues/3357)

### Safe math functions

There are a variety of functions that can throw an error in some databases, for instance divide-by-zero, numeric overflow, and taking the square root of a negative number. In these database, the error manifests itself as a error message transmitted to the analyst, which can be exploited by an attacker (see [Error generation attacks](#error-generation-attacks)).

For these databases, an exception handler for each such function is installed in the database, either by the cloak when it connects, or through configuration of the database prior to cloak connection. These exception handlers prevent an error from being transmitted to the analyst. See [Determine if safe math functions needed](#determine-if-safe-math-functions-needed).

### Per column min and max values

The [safe math functions](#safe-math-functions) unfortunately slow down query execution. To mitigate this, the cloak conservatively estimates when a math function *might* result in an exception, and only executes the safe functions in these cases.

In order to make this estimate for numeric [overflow](#overflow) exceptions, the cloak records a minimum and maximum value for each numeric column. These recorded values are not the true minimum and maximum values, because an attacker could then detect these values through a series of queries that detect when a safe function was executed through a timing attack.

To prevent this, the recorded min and max are not the true min and max, but rather an approximated min and max that with high (but not 100%) probability exceed the true min and max. The approximated min and max are computed as follows:

1. Take the top/bottom 1000 values from a given column (note that a given user can have multiple values)
2. Discard all but the biggest/smallest value for each user out of those
3. Randomly select a cutoff with configured mean (default 20) and stddev (default 5)
  * The cutoff won't be lower than a configured min (default 10)
  * The PRNG for the cutoff is seeded with the table and column name, giving the same result on every run
4. Ignore a number of the top/bottom values from step 2 equal to the selected cutoff
5. Take the top/bottom value after that and find the closest snapped value above/below it (see [Snapped Ranges](#snapped-ranges))
6. Given both a min and max computed this way create an extended bound
  * If min is negative while max is positive, multiply both by 10
  * If both are positive, multiply max by 10 and divide min by 10
  * If both are negative, multiply min by 10 and divide max by 10
7. If there are not enough values to compute either min or max, set the bounds to :unknown and always use safe math functions

# Handle incoming SQL

## Supported SQL

The following shows what SQL is supported. Any received SQL that has syntax outside of this specification is rejected.

```sql
SELECT [DISTINCT | ALL]
  field_expression [, ...]
  FROM from_expression [, ...]
  [ WHERE filter_expression [AND ...] ]
  [ GROUP BY column_expression | position [, ...] ]
  [ HAVING filter_expression [AND ...] ]
  [ ORDER BY column_name  | position [ASC | DESC] [NULLS FIRST | LAST] [, ...] [ LIMIT amount ] [ OFFSET amount ] ]
  [ SAMPLE_USERS <0..100>% ]

field_expression :=
  * | table_name.* | column_expression [AS alias]

column_expression :=
  [table_name.]column_name |
  aggregation_function([DISTINCT | ALL] column_name) |
  function(column_expression) |
  column_expression binary_operator column_expression |
  column_expression::data_type

binary_operator :=
  + | - | * | / | ^

data_type :=
  integer | real | text | boolean | datetime | date | time

from_expression :=
  table | join

table :=
  table_name [[AS] alias] | (select_expression) [AS] alias

join :=
  table CROSS JOIN table |
  table { [INNER] | { LEFT | RIGHT } [OUTER] } JOIN table ON filter_expression

aggregation_function :=
  COUNT | SUM | AVG | MIN | MAX | STDDEV | VARIANCE | MEDIAN |
  COUNT_NOISE | SUM_NOISE | AVG_NOISE | STDDEV_NOISE | VARIANCE_NOISE

filter_expression :=
  column_expression equality_operator (value | column_expression) |
  column_expression inequality_operator (numerical_value | datetime_value) |
  column_expression BETWEEN value AND value |
  column_expression IS [NOT] NULL |
  column_expression [NOT] IN (constant [, ...]) |
  column_expression [NOT] LIKE | ILIKE string_pattern [ESCAPE escape_string] |
  column_expression [NOT] boolean_column_expression
  column_expression

comparison_operator :=
    equality_operator | inequality_operator

equality_operator :=
    = | <>

inequality_operator :=
    > | >= | < | <=
```

## Rejecting queries

Even if the SQL satisfies the above syntax, there are a number of specific uses that can be rejected.

### Inequalities

While the cloak allows inequality operators (`> | >= | < | <=`), there are restrictions on how they may be used. In general, the cloak requires that both sides of an inequality be specified. This is necessary to prevent difference attacks using [range creep with averaging](#range-creep-with-averaging).

This document refers to all inequalities that are bounded on both sides as *ranges*.

In addition to the ranges that can be specified with `BETWEEN` and inequality operators, some functions implicitly define a range. For instance, the function `hour` defines a range of width one hour. The definition of range in this document includes implicit ranges. These are: `hour`, `minute`, `second`, `year`, `quarter`, `month`, `day`, `weekday`, `date_trunc`, `round`, `trunc`, and `bucket`.

### Clear conditions (IN, range, negative conditions)

The cloak may require that certain conditions are *clear*. The primary purpose of clear conditions is so that the cloak can [seed noise layers](#determine-seeds) through [SQL inspection](#sql-inspection) rather than by [floating the column value](#floating-columns-values). This is necessary for the following operators, which cannot be easily floated:

* negative conditions (`col <> val`) including `NOT IN`
* `IN` (`col IN (val1,val2)`)
* range (`col BETWEEN val1 and val2`), including implicit ranges.

The term "clear" implies that it is clear from SQL inspection alone what the semantics of the conditions are, and therefore how to seed.

Clear conditions also have the effect of reducing the attack surface since it gives an attacker fewer mechanisms to work with. For isolating columns, the cloak forces clear conditions for all operators. In this sense, the notion of clear is overloaded. 

`IN` and range are limited to only constants on the right hand side (rhs). Negative conditions may additionally have columns on the rhs.

The left hand side (lhs) can have either the native column (no transformations), or the native column with a single instance of the following functions:

For text columns, or columns that have been cast as text: `lower`, `upper`, `trim`, `ltrim`, `btrim`, and `substring`.

For date, datetime, and time columns: `hour`, `minute`, `second`, `year`, `quarter`, `month`, `day`, `weekday`, and `dow`.

A column in a clear condition cannot have undergone transformations prior to the condition. For instance, in the following query, the `IN` condition must be clear, but since there is a prior transformation (`numeric + 1` in the sub-query), the condition is unclear and the query rejected.

```sql
SELECT COUNT(*)
FROM (
    SELECT numeric + 1 AS number
    FROM table) x
WHERE number IN (1, 2, 3)
```

### Too much math

#### In general

In order to prevent [SQL backdoor attacks](#sql-backdoor-attacks), the cloak limits the complexity of certain math in queries. We are particular concerned with discontinuous functions combined with constants because they can be coerced into acting as discrete logic. However, there are many ways of obtaining discontinuous functions, for instance string manipulations (e.g. `left`, `ltrim`) followed by casting to numbers, or datetime functions (e.g. `year` or `hour`). In addition, functions can be coerced into constants, as for instance `pow(col1, col2-col2)` is 1. 

We take a conservative approach and limit the number of expressions containing a restricted function and a constant, or more than one restricted function to a total of 5.

The restricted functions and operators include `+`, `-`, `/`, `*`, `^`, `%`, `abs`, `ceil`, `floor`, `length`, `round`, `trunc`, `btrim`, `left`, `ltrim`, `right`, `rtrim`, `substring`, `year`, `quarter`, `month`, `day`, `weekday`, `hour`, `minute`, `second`, and `date_trunc`.

#### With isolating columns

In order to prevent [Linear program reconstruction](#linear-program-restruction) attacks, the cloak places even more restrictions on expressions associated with [isolating columns](#isolating-column-label). An isolating column is one where 80% or more of the values in the column are associated with only a single user. These columns are susceptible to linear program reconstruction attacks because they may be used to select groups of individual users.

The cloak requires *all conditions*, including `=`, that operate on isolating columns to be [`clear`](#clear-conditions).

### String functions

In order to generally reduce the attack surface available with string functions (`lower`, `upper`, `trim`, `ltrim`, `btrim`, and `substring`), the following limitations apply:

1. Columns which have undergone a string functions cannot be combined with other transformations.
2. String functions cannot be applied to columns that have undergone multiple casts.
3. Results of string functions can only be compared with constants or with other columns.

### LIKE and NOT LIKE

`[NOT] [I]LIKE` have similar restrictions as string functions. The first two restrictions that apply to [String functions](#string-functions) apply to `[NOT] [I]LIKE` as well (cannot be combined with other transformations, cannot be used with columns that have mutiple casts).

The regex associated with `[NOT] [I]LIKE` is limited to using only the `%` wildcard (`_` is not allowed), and only in the first or last position of the regex. For example, `col LIKE '%stuff%'` is allowed, but not `col LIKE 'stu%ff'`.

### Other limitations of isolating columns

Besides requiring that all conditions operating on isolating columns are clear, `IN` with more than one element on the rhs is not allowed. (`IN` with one element is equivalent to `=`).

This is disallowed to prevent [noise exploitation attacks through chaff conditions](#through-chaff-conditions).

### Limitations due to shadow table

To mitigate [noise exploitation attacks through chaff conditions](#through-chaff-conditions), negative conditions and `IN` rhs elements are disallowed if the corresponding constant does not appear in the shadow table.

Note that this mechanism is not effective in all cases. The shadow table is based on the complete column. Any given query, however, may have conditions that cover only part of the column.  A constant may therefore appear in the shadow table and still not have any matching rows in the context of a given query. For instance, the condition `gender <> 'F'` may not be chaff in the `pro_football_players` table, but may be chaff when combined with `tournament = 'fifa world cup'`.

### Conditions with two columns

The cloak allows conditions with two columns. However, each column may appear only once in the condition.  Conditions with more than two columns are not allowed.

Two-column conditions with inequalities do not require that the condition be a range (i.e. have both a lower and upper boundary). For instance, the condition `WHERE col1 < col2` is allowed.

## Modifying queries

Normally queries with SQL that does not conform to the requirements of the cloak are rejected. Cases where the query is modified rather than rejected are described here.

### Snapped Ranges

In order to prevent prevent difference attacks using [range creep with averaging](#range-creep-with-averaging), all ranges must conform to pre-designated widths and offsets.

In the case of numeric data types, the widths must fall within the following infinite sequence of widths: `[..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, ...`]. The offset must fall on an even multiple of the width, or an even multiple plus 1/2 of the width. The following are allowed:

* `col BETWEEN 10 and 15`: width 5, offset 10
* `col BETWEEN 7.5 and 12.5`: width 5, offset 7.5 (shifted by 2.5)
* `col BETWEEN -0.001 and -0.002`: width 0.001, offset -0.002

The following are not allowed:
* `col BETWEEN 10 and 13`: width 3 (disallowed width)
* `col BETWEEN 8 and 13`: width 5, offset 8 (not multiple of 5 or 2.5)

In the case of datetime types, the widths must correspond to natural datetime boundaries (year, month, week, day, hour, minute, second). Within each such boundary, the following additional widths are allowed:

* `[1, 2, 5, 10, 20, 50, ...]` years
* `[1, 2, 6, 12]` months
* `[1, 2, 5, 10, 20]` days
* `[1, 2, 6, 12, 24]` hours
* `[1, 2, 5, 15, 30, 60]` minutes
* `[1, 2, 5, 15, 30, 60]` seconds

# Aggregation functions sum, min, max, count

Different anonymization mechanisms are used for different aggregation functions. In this section, we describe anonymization for `sum(col)`, `count(*)`, `count(col)`, `count(DISTINCT uid)`, `min(col)`, and `max(col)`. A key requirement here is to minimize the amount of data that is transferred from the database to the cloak. The implementation of these functions limits the transfer to one row of data per bucket.

## Generate DB query

As a running example for this section, we use the following query submitted by an analyst to the cloak:

```sql
SELECT frequency, sum(amount) AS amount_sum
FROM transactions
WHERE abs(acct_district_id) = 1
GROUP BY 1
```

The query subsequently generated by the cloak for the database is in essence the following (simplified for readability):

```
     1	SELECT frequency,
     2	       SUM(amount_sum) AS amount_sum,
     3	       COUNT(uid) AS count_duid,
     4	       MIN(uid) AS min_uid,
     5	       MAX(uid) AS max_uid,
     6	       COUNT(amount_sum) AS amount_count,
     7	       MIN(amount_sum) AS amount_min,
     8	       MAX(amount_sum) AS amount_max,
     9	       STDDEV(amount_sum) AS amount_stddev,
    10	       MIN(acct_district_id_min) AS acct_district_id_min,
    11	       MAX(acct_district_id_max) AS acct_district_id_max,
    12	       SUM(acct_district_id_cnt) AS acct_district_id_cnt
    13	FROM
    14	  (SELECT uid, frequency, sum(amount) AS amount_sum,
    15	          MIN(acct_district_id) AS acct_district_id_min,
    16	          MAX(acct_district_id) AS acct_district_id_max,
    17	          COUNT(*) AS acct_district_id_cnt
    18	   FROM transactions
    19	   WHERE ABS(acct_district_id) = 1
    20	   GROUP BY uid, frequency) t1
    21	GROUP BY frequency
```

### Gather bucket statistics

In order to compute the noise for aggregate answers and the noisy threshold for suppression, the cloak needs to have certain statistics about how much users contribute to each aggregate, especially users that contribute the most. The statistics are:

* `avg(contribution)`
* `min(contribution)`
* `max(contribution)`
* `stddev(contribution)`

This information is obtained by:

1. Adding the `uid` column to the selected columns (line 14) and `GROUP BY` (line 20),
2. Computing the contribution per user (`amount_sum` in line 14, since the aggregate in this example is `sum(amount)`,
3. Computing the four statistics (`amount_min` line 7, `amount_max` line 8, `amount_stddev` line 9, and as part of computing the average, `amount_count` line 6).

On line 14, `sum(amount)` would be replaced with `count(*)`, `count(amount)`, or `1` if the analyst aggregate is `count(*)`, `count(amount)`, and `count(DISTINCT uid)` respectively.

### Gather seed materials

Diffix adds noise samples (i.e. layers) per query filter condition. There are two types of noise layers, UID-noise and static-noise. Static-noise is "sticky" in the sense that the same filter condition, i.e. `age = 20`, will typically generate the same noise sample. UID-noise is sticky in the sense that the same filter condition combined with the same set of selected users will typically generate the same noise sample.

The mechanism for generating the same noise sample is to generate the same seed for the PRNG (Pseudo-Random Number Generator). Each seed is composed of several components which we call the *seed materials* (see [Determine seeds](#determine-seeds)).

The seed material for both the UID-noise and the static-noise contain the values that are filtered for (in the case of positive conditions) or filtered against (in the case of negative conditions) by the condition. In some cases, it is possible to determine that value through inspection of the SQL itself. For instance, the selected value of `age = 20` is 20. In other cases, however, it is not clear from inspection, and so the selected value or values are requested from the database. We refer to this as "floating" the column.

In our running example, there are two filter conditions. One is in the `WHERE` clause (`ABS(acct_distrit_id) = 1`, line 19), and one is the selected column `frequency` (line 1). In this case the cloak does not interpret the filtered value for the `WHERE` condition by inspection, but instead floats the column `acct_district_id`.

The static-noise is seeded by the minimum and maximum column values, and so these must be floated. In the running example, lines 15 and 16 select the per-UID per-bucket min and max for column `acct_district_id`, and lines 10 and 11 select the per-bucket min and max.
[ghi3274](https://github.com/Aircloak/aircloak/issues/3274)

The uid-noise is seeded by (among other things):
1. the minimum UID (line 4),
2. the maximum UID (line 5),
3. the distinct number of UIDs (line 3), and
4. the number of rows (lines 12 and 17).

### Determine if safe math functions needed

A conservative analysis is run to determine if a given math expression in the query might result in an exception. Examples would be determining if a column in a divisor could be zero, or determining whether a `pow()` function on a column could lead to overflow. The analysis makes the assumption that the column value from the database does not exceed the approximated min and max (see [Per column min and max values](#per-column-min-and-max-values)). 

If a math function might lead to an exception, then the function in the query is replaced with a safe version (see [Safe math functions](#safe-math-functions)).

If the analysis determines that a math function cannot lead to an exception, then the possible values for the column are bounded in the query so that the min and max assumptions are correct. An example of how this is done is as follows:

```
CASE WHEN amount < 0 THEN 0
     WHEN amount > 1000000 THEN 1000000
     ELSE amount
END
```

Here the assumed min and max for the `amount` column are 0 and 1000000. The `amount` column is replaced with this `CASE` statement, ensuring that any value from the `amount` column does not exceed these bounds.

For simplicity, these substitutions are not shown in the [example database query](#generate-db-query).

### Add protection against JOIN timing attack

As described in the [JOIN timing attack](#join-timing-attack), an analyst can strongly influence query execution time in the database by including `JOIN` expressions that may return an empty table.

To prevent this, the cloak modifies all but the last JOIN expression so that at least one row is always returned. This is done with a `UNION` operation:

```
original_expression UNION inverse_original_expression
```

The second expression of the `UNION` returns a single row if and only if the first expression, which is the original expression, returns no rows. Following is an example:

```
SELECT uid
FROM accounts
WHERE lastname = 'Zamora'
  AND birthdate = '1996-11-16'
UNION ALL SELECT -2147483648
WHERE NOT EXISTS
(
   SELECT uid
   FROM accounts
   WHERE lastname = 'Zamora'
     AND birthdate = '1996-11-16'
)
```

The original `JOIN` expression is the part above the `UNION`. It is repeated within the `WHERE NOT EXISTS` expression. The forced UID `-2147483648` is chosen to be a value that is very unlikely to exist in practice. As a result, if the forced UID is selected, it won't match anything from the other `JOIN` expressions and so won't effect the answer.

### Add protection against divide-by-zero attacks

To defend against [Divide by zero](#divide-by-zero) attacks, the cloak modifies any expression having a column in the denominator of a divide operation so that NULL is generated instead of an error exception.

This is done using a `CASE` statement that detects when the denominator is zero (or nearly so), and substitutes a NULL. For example, in the condition:

```
WHERE 1/column = 0.1
```

the expression `1/column` is substituted with:

```
CASE WHEN column < 1.0e-100 THEN NULL
     ELSE 1/column
END
```

### Add protection against square root of negative numbers

To defend against [Square root of a negative number](#square-root-of-a-negative-number) attacks, the cloak modifies any expression having a column in a square root opeation that NULL is generated instead of an error exception.

This is done using a `CASE` statement that detects when the expression in the square root operation is negative, and substitutes a NULL.  For example, in the condition:

```
WHERE sqrt(column) = constant
```

the expression `column` is substituted with:

```
CASE WHEN column < 0 then NULL
     ELSE sqrt(column)
END
```

## Handle DB answer

### Noise Layers

A noise layer is an individual Gaussian noise sample. The total noise added to an aggregate value in each bucket of a query answer is the sum of the individual noise samples. Although noise layers are taken from a Gaussian distribution, their values are not random. Rather, the PRNG used to generate each noise sample is seeded in such a way that identical query or answer conditions generate the same noise sample. We refer to this property as being *sticky*.

There are two types of noise layers, *static* and *uid*. The difference between them is based on what is used to seed each layer. Static noise layers typically (though not always) depend only on the semantics of the individual SQL query filter condition. In other words, the `WHERE` condition `age = 10` will always produce the same static noise layer independent of what UIDs comprise the answer. By contract, UID noise layers always depend on which UIDs conprise the answer.

Each filter condition has a static noise layer, and most filter conditions additionally have a UID noise layer. The following are considered to be filter conditions:

* WHERE clauses
* HAVING clauses in subqueries
* GROUP BY clauses
* All columns in the top-level SELECT clause
* Range WHERE clauses, i.e. a pair of `a >= x` and `a < y`, is considered one filter instead of two
* Each element of `IN(a,b,c)` is treated as one distinct filter condition, while the entire expression taken together is treated as another filter condition

In addition, there is a generic noise layer for queries that otherwise have no noise layer (i.e. because no filter conditions). The query `SELECT count(*) FROM table` is such a query.

The aggregation function `count(col)` is given an additional UID noise layer. The purpose of this noise layer is to defend against the [Difference attack with counting NULL](#difference-attack-with-counting-null).

Finally, conditions comparing two columns (`col1 <>|<|<=|>|>= col2`) have no noise layers

### Determine seeds

<!----
TODO: This section is probably not quite right or complete.
I'm not sure about how implicit ranges are seeded, for instance.
---->

Upon receiving the database answer, for each one-column condition, the cloak knows the following information:

1. column name
2. table name

For each bucket, the cloak additionally knows:

3. min column value
4. max column value
5. min UID
6. max UID
7. count of distinct UIDs
8. count of rows

The min and max column values are obtained either by floating (see [Gather seed materials](#gather-seed-materials)), or by inspection of the condition itself in the query, as follows:

* If `col = val` or `col <> val`, then both the min and max are set to `val`.
* If `col BETWEEN val1 AND val2`, then min is set to `val1`, and max to `val2`.
* If `col IN (val1, val2, ...)`, this is transformed into multiple individual conditions `col = val1`, `col = val2`, ..., and min and max values are set accordingly.
* If a selected column (`SELECT col FROM ...`), then min and max are set to the value returned by the database.
* If an implicit range, then min is set to the left edge of the range, and max is set to the right edge of the range.
* In cast of text datatypes, `val` is converted to lower case (i.e. `lower(val)`) for the purpose of seeding.

Most noise layers are seeded with at least the following:

* A canonical name of the column in the form {"table", "column"}
* A secret salt (random value) that is established when the cloak is initialized and not changed afterwards
* The min and max column value

The negative conditions `<>`, `NOT LIKE`, and `NOT ILIKE` add the symbol `:<>` to the seed material. Note that `NOT IN` conditions are converted to their equivalent `<>` forms.

UID noise layers additionally have the minimum and maximum UID values in the seed material.

The additional UID noise layer assigned to `count(col)` is seeded with the secret salt, the canonical column name, and the minimum and maximum UID values.

The generic noise layer is seeded with the `count(DISTINCT uid)` value and the secret salt.

### Low count suppression

The cloak obtains the number of distinct UIDs that contribute to each bucket (see [Gather seed materials](#gather-seed-materials)). Buckets that have too few distinct UIDs are suppressed (not returned to the analyst in the query answer).

A noisy threshold is used to determine if a bucket should be suppressed. The threshold value is determined by seeding a Gaussian random number with:

* Minimum UID
* Maximum UID
* Number of distinct UIDs

using the paramters `mean = 4` and `stddev = 0.5`. In the example of section [Generate DB query](#generate-db-query), these three aggregates are gathered in lines 3-5.

If the number of distinct UIDs is less than the threshold value, then the bucket is suppressed.

### Value flattening and noise addition

The cloak perturbs aggregate function values. This section describes how the `sum(col)`, `count(*)`, `count(col)`, `count(DISTINCT uid)`, `min(col)`, and `max(col)` aggregates are perturbed.

The magnitude of perturbation is related to how much individual users contribute to the aggregate. The magnitude depends on two types of users, *heavy contributors*, and *extreme contributors*.

Extreme contributors are defined as the 1 or 2 users with the highest contribution. Heavy contributors are defined as the 3 to 5 users with the next highest contribution.

Conceptually the perturbation should do the following. First, it should modify the contributions of the extreme contributors so that they are similar to those of the heavy contributors. Second, it should add noise proportional to the magnitude of the heavy contributions.

For example, suppose a column contains user salary. Suppose that the average salary is 100K, but there are four or five users with salaries between 450K and 550K, and only one user with a salary higher than 550K. That user's salary is 2M. The group of four or five users would be regarded as heavy contributors, and the user with 2M salary would be regarded as an extreme contributor.

We want to adjust the value of the extreme contribution (2M) so that it is more like those of the heavy contributors. If the aggregate is `sum()`, this effectively means reducing the reported sum by around 1.5M. We call this adjustment "flattening". We want to then add noise proportional to that of the heavy contributors, i.e. proportional to around 500K.

The rational for these perturbations is as follows. Assume that an analyst is able to forumlate two queries, one whose answer does not include the victim (the left query), and one that may include the victim (the right query). The analyst wants to compare answers to determine if the victim is present in the right query or not. If the right answer is greater than the left answer, for instance, the analyst may assume that the victim is present in the right answer. This is the basic [Difference attack](#difference-attacks).

There needs to be enough noise that the analyst's confidence in a guess as to the presence or absence of the victim in the right query is low.

Alternatively, we can assume a worst-case where an analyst knows every value in a column except that of a single user (the victim), and the analyst wants to determine if the victim is included in the database or not. If a query taking the sum of the column is greater than the sum of all known values, then again the analyst can assume that the victim is in the database, and again there needs to be enough noise to keep the analyst's confidence low.

Since the victim may be a heavy contributor, there needs to be enough noise to obscure the presence or absence of a heavy contributor. The analyst can, however, in many cases estimate the magnitude of noise that was added. Indeed, the cloak explicitly reports this to the analyst for the purpose of better understanding the cloak's answer. If there is an extreme contributor, then merely the magnitude of noise can reveal with high confidence the presence or absence of the user.

To prevent this, the cloak flattens the extreme user or users values to hide the extreme user by blending it in with the heavy users. The noise is then based on the average contributions of the heavy users. Unfortunately, the cloak cannot report how much flattening took place, since that would defeat the purpose. As a result, aggregate values can on occasion be very inaccurate, and the analyst needs to be aware of this possibility.

In Diffix Birch, the perturbations for extreme and heavy contributors were done explicitly. That is, the cloak would explicitly determine the contributions of each user, identify the extreme and heavy contributors, and compute the flattening and noise accordingly. This process would sometimes incur a heavy performance penalty, as the database would have to convey per-user information to the cloak.

To improve performance, Diffix Cedar uses per-bucket rather than per-user information. In most cases, this substantially reduces the amount of data transferred from the database to the cloak. Unfortunately, this means that the cloak can no longer explicitly determine the extreme and heavy contributors. Instead, the cloak uses aggregate statistics about the users in each bucket to *estimate* the extreme and heavy contributors.

The statistics are these (line numbers taken from [example query](#generate-db-query)):

* `col_avg`: average contribution of all users (line 6 divided by line 3)
* `col_std`: standard deviation of users' contributions (line 9)
* `col_min`: min contribution of all users (line 7)
* `col_max`: max contribution of all users (line 8)
* `col_cnt`: the number of distinct UIDs of the data (line 3)

These user statistics are taken for each aggregate function in the query.

For each noise layer, a Gaussian sample is generated with `mean=0` and standard deviation `sd=1` (as seeded in [Determine seeds](#determine-seeds)). These are summed together to produce the `base_noise`.

#### Operation of sum

The following algorithm describes the algorithm for computing perturbation for `sum(col)`, `count(*)`, `count(col)`, and `count(DISTINCT uid)`. The same algorithm works for all four aggregate functions.  `count(*)` is the sum where each row has value 1. `count(col)` is the sum where each non-NULL row has value 1 and NULL rows have value 0. `count(DISTINCT uid)` is the sum of users where each user contributes value 1.

The perturbation for sum is:

`perturbation_sum = (base_noise * sum_sd) - flatten`

where `sum_sd` is a factor applied to the `base_noise` and is approximately proportional to the average heavy contribution, and `flatten` is the adjustment due to an approximation of the difference between the extreme contributions and the average heavy contribution.

Note that for `count(DISTINCT uid)`, all users contribute exactly the same, so `flatten=0`, and `sum_sd=1`.

The sum advertised to the analyst is:

`noisy_sum = true_sum + perturbation_sum`

where `true_sum` is the exact (non-noisy) sum from the database (line 2).

If we assume for the moment that all user contributions are positive, then the intuition behind the algorithm follows from these two observations:
1. The larger the standard deviation (`col_std`), the larger the average heavy contribution.
2. The larger the max value relative to the average plus several standard deviations, the greater the amount of flattening.

This is perhaps explained through several examples. Let's suppose that these examples are for `sum(salary)`.

First, consider a case where `col_min=col_max=col_avg=100K` and `col_std=0`. In this case, all users have the same salary of 100K, we want a noise factor of `sum_sd=100K` (i.e. equal to the contribution of any user), and there is no flattening.

Now suppose that `col_min=95K`, `col_avg=100K`, `col_std=3K`, and `col_max=10M`. The relatively low standard deviation `col_std=3k` implies that the majority of salaries are clustered tightly around the average of `col_avg=100K`. In contrast to this, however, the maximum salary `col_max=10M` is far away from this cluster, suggesting that a great deal of flattening is needed.

Now suppose `col_min`, `col_avg`, and `col_max` stay the same as in the previous example, but `col_std=200K`. This implies that the salaries are no longer so tightly clustered around the average, but rather there are some relatively high salaries that can be regarded as heavy hitters. As the standard deviation increases, all other things being equal, the noise factor `sum_sd` would increase, and the amount of flattening would decrease.

The following algorithm was generated through a process of trial and error, with a goal towards minimizing perturbation while keeping attacker confidence low.

The algorithm has three constants, set as follows:

* `factor=4`
* `avg_scale=1`
* `top_scale=0.5`

The algorithm operates over positive values, negative values, or a combination. The algorithm actually makes two computations, one for values above the average, and one for values below the average. `sum_sd` is the largest of the two `sum_sd` values computed above and below. By contrast, `flatten` is a combination of the flattening for above and below. (For a distribution with positive values, almost all of the flattening comes from the computation above. For distributions with both positive and negative values, however, flattening above and below can in fact cancel each other out!)

Of course, the `col_std` computed by the cloak is comprised of values both above and below the `col_avg`. So we use the `col_max` and `col_min` to estimate how much of the `col_std` comes from above and how much comes from below, as follows:


```
std_above = col_std * ((col_max - col_avg)/(col_max - col_min))
std_below = col_std * ((col_avg - col_min)/(col_max - col_min))
```

Next we try to estimate the rough values for heavy contributors above and below. These are computed using the estimated standard deviations and `factor=4` constant:

```
heavy_above = col_avg + (factor * std_above)
heavy_below = col_avg - (factor * std_below)
```

```
flatten_above = col_max - heavy_above
flatten_below = col_min - heavy_below
```

Note that either flatten value may be positive or negative.

The final `flatten` value is then:

```
flatten = flatten_above + flatten_below
```

Flattening mimics the effect of modifying values in the data. This in turn should impact the computed average. To reflect this impact, we adjust `col_avg` as follows:

```
if (flatten > 0) {
  col_avg = col_avg - (flatten / col_cnt)
}
```
Finally, `sum_sd` is computed as:

```
sum_sd = max(
  abs(avg_scale * col_avg),
  abs(top_scale * heavy_above),
  abs(top_scale * heavy_below)
)
```

Remembering that `avg_scale=1` and `top_scale=0.5`, the above computation was found to reduce overall noise while keeping attacker confidence low.
[ghi3013](https://github.com/Aircloak/aircloak/issues/3013)

#### Operation of avg

The average aggregator is computed as the `sum(col)`/`count(col)`.

#### Operation of min and max

The aggregators `min(col)` and `max(col)` are set to the `edge_below` and `edge_above` values from [Operation of Sum](#operation-of-sum), with the exception that `min(col)` may never be greater than `avg(col)`, `max(col)` may never be less than `avg(col)`, and `min(col)` may never be greater than `max(col)`. 

### Reporting suppression

In order to help analysts understand query results, the cloak provides information about suppressed buckets. Data from suppressed buckets are merged to generate a new bucket, referred to as a *star* bucket. Star buckets are subject to the same bucket suppression and noise adding mechanisms as any bucket. In other words, a star bucket itself may be suppressed. As such, absence of a star bucket does not necessarily imply that no suppression has taken place.

The following simplistic and idealized example illustrates how suppression is reported. Here `count` is a count of distinct UIDs. For simplicity, we assume here that buckets with 4 or fewer distinct UIDs are suppressed. In reality, this decision is based on a noisy threshold ([Low count suppression](#low-count-suppression)). This example also assumes that any given UID appears only once. As a result, the count resulting from merging two buckets is the sum of the two individual counts.

Suppose that the data transferred from the database to the cloak is as follows:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   a   |   1   |  10   |
|   a   |   2   |   2   |
|   a   |   3   |   3   |
|   b   |   2   |   7   |
|   b   |   4   |   8   |
|   b   |   1   |   4   |
|   b   |   7   |   3   |
|   b   |   9   |   4   |
|   b   |   5   |   4   |
|   c   |   1   |   3   |
|   d   |   2   |   3   |

Two columns, `x` and `y` have been selected.

The suppressed buckets are these:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   a   |   2   |   2   |
|   a   |   3   |   3   |
|   b   |   1   |   4   |
|   b   |   7   |   3   |
|   b   |   9   |   4   |
|   b   |   5   |   4   |
|   c   |   1   |   3   |
|   d   |   1   |   3   |

Suppression reporting attempts to expose as many column values as possible. It gives priority to reporting left-hand column values. As such, the suppressed buckets are merged as follows:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   a   |   *   |   5   |
|   b   |   *   |  15   |
|   c   |   *   |   3   |
|   d   |   *   |   3   |

The `x` values are preserved, and the `y` values are replaced with a star (`*`) symbol. The first two buckets will be reported to the analyst, but the second two buckets must themselves be suppressed, and so are extracted:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   c   |   *   |   3   |
|   d   |   *   |   3   |

These buckets are merged into the following bucket, losing the `x` column values in the process:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   *   |   *   |   6   |

This bucket is not suppressed, and so is reported to the analyst.

The final set of buckets is therefore:

|   x   |   y   | count |
| :---: | :---: | :---: |
|   a   |   1   |  10   |
|   a   |   *   |   5   |
|   b   |   2   |   7   |
|   b   |   4   |   8   |
|   b   |   *   |  15   |
|   *   |   *   |   6   |

These buckets are reported to the analyst with noisy counts.

Three of the seven 'y' values have been suppressed, and two of the four x values. Had the query placed the `y` column on the left, then the suppression process would have produced this:

|   y   |   x   | count |
| :---: | :---: | :---: |
|   1   |   a   |  10   |
|   1   |   *   |   7   |
|   2   |   b   |   7   |
|   2   |   *   |   5   |
|   4   |   b   |   8   |
|   *   |   *   |  14   |

Note that somewhat more detail regarding the `y` column values is revealed.

Although this example used the star symbol for both the text and integer column, in practice it would be illegal to return a star symbol, which is text, in a numeric or datetime column. In these cases, the cloak would report a `NULL` value.

#### Bucket merging

Merged buckets are processed like any other bucket: they are [checked for suppression](#low-count-suppression) and [aggregate answers are perturbed](#value-flattening-and-noise-addition). As such, the information needed for this processing must be preserved when buckets are merged (i.e. lines 2-9 in the [example query](#generate-db-query)).

It is not possible to preserve this information perfectly. Merging is essentially an `OR` operation, and as such any users that happen to be in both buckets cannot be accounted for. Therefore the merge is designed to produce approximate values.

The information that needs to be generated is: `col_sum, count_duid, min_uid, max_uid, col_count, col_min, col_max, col_stddev`. The cloak also records the set of UIDs (`uid_set`) generated in previous merges during the processing of an answer. Assuming the merging of two buckets with subscripts 1 and 2, the values for the merged bucket are computed as follows:

- `merged_uid_set = union(uid_set1, uid_set2)`
- `merged_col_sum = col_sum1 + col_sum2`
- `merged_col_min = min(col_min1, col_min2)`
- `merged_col_max = max(col_max1, col_max2)`
- If the two uid ranges do not overlap: 
  - `merged_col_count = col_count1 + col_count2`
  - `merged_count_duid = count_duid1 + count_duid2`
- If the two uid ranges touch (minimum of one set equals the maximum of the other set):
  - `merged_col_count = col_count1 + col_count2 - 1`
  - `merged_count_duid = count_duid1 + count_duid2 - 1`
- Otherwise, merged count is estimated as the maximum count plus a quarter of the minimum count (because some collisions could occur):
  - `merged_col_count = max(col_count1, col_count2) + min(col_count1, col_count2) / 4`
  - `merged_count_duid = max(count_duid1, count_duid2) + min(count_duid1, count_duid2) / 4`
- `merged_avg = merged_col_sum / merged_col_count`
- For the standard deviation, we use the formula: `sd(v) = sqrt(sum(v^2) / count - avg(v)^2)` We first extract the sums of squared values: `sum_sqrs1 = (col_stddev1^2 + avg1^2) * col_count1` and `sum_sqrs2 = (col_stddev2^2 + avg2^2) * col_count2`, we then add them together to get the merged sum of squared values, resulting in: `merged_sd = sqrt(merged_sum_sqrs / merged_col_count - merged_avg^2)`


# Aggregation function count distinct

When computing for instance `count(column)`, any non-NULL column value will contribute to the count. Noise must be added for instance to hide the fact that a single user may be included or excluded.

By contrast, when computing `count(DISTINCT column)`, it is not always necessary to add noise. This is because including or excluding a single user may not change the count at all. To take a simple example, suppose that the column is `gender` with possible values 'M' and 'F', and that both genders are well-represented by many users. The answer to `count(DISTINCT gender)` is `2`, and including or excluding any given user does not change that answer. In cases like this we want to return the exact answer.

Suppose on the other hand the column is `purchase_time`, that there are many thousands of different purchase times, and that many of the purchase times are unique to a single user. In this case, the presence or absence of a single user can easily change the true answer, and so noise is needed to hide such users.

To deal with this, the cloak generates a query that first determines, for each UID, the count of column values `count_distinct_vals_per_uid` that are unique to that UID. The cloak then computes the `min`, `max`, `avg`, and `stddev` statistics across all `count_distinct_vals_per_uid`, and uses the same algorithm as with `sum()` to [compute noise and flattening](#value-flattening-and-noise-addition). If there are zero column values that have a single UID associated with them, then no noise is added.
[ghi3563](https://github.com/Aircloak/aircloak/issues/3563)
[ghi4080](https://github.com/Aircloak/aircloak/issues/4080)

The following example shows how this is done.

Suppose the analyst query is:

```SQL
SELECT column1, COUNT(DISTINCT column2) FROM table GROUP BY 1
```

The cloak would generate a query similar to the following simplified example:

```
     1	SELECT
     2	  group_0,
     3	  MAX(CAST((uid IS NOT NULL) AS integer) * count_distinct) AS cnt_max,
     4	  MIN(CAST((uid IS NOT NULL) AS integer) * count_distinct) AS cnt_min,
     5	  AVG(CAST((uid IS NOT NULL) AS integer) * count_distinct) AS cnt_avg,
     6	  STDDEV(CAST((uid IS NOT NULL) AS integer) * count_distinct) AS cnt_stddev,
     7	  SUM(count_distinct) AS count_distinct
     8	FROM (
     9	  SELECT
    10	    user_id,
    11	    group_0,
    12	    COUNT(target) AS count_distinct
    13	  FROM (
    14	    SELECT
    15	      column1 AS group_0,
    16	      column2 AS target,
    17	      CASE WHEN (COUNT(DISTINCT user_id) < 3) THEN MIN(user_id) ELSE NULL END AS user_id
    18	      FROM table
    19	      GROUP BY 1, 2
    20	  ) AS distinct_values
    21	  GROUP BY 1, 2
    22	) AS uid_grouping
    23	GROUP BY 1
```

The inner-most `SELECT` (lines 14 - 19) does a `GROUP BY` based on distinct values of the selected columns. In line 17, the UID of values that have only one user is recorded.

In the next `SELECT` (lines 9 - 21), at line 12 we count the number of times each UID appears, as well as the number of times no UID appears (which gives us the number of distinct column values that have more than one user).

In the outer-most `SELECT`, the maximum, minimum, average, and stddev statistics for how often each UID is unique for a `column1` value are computed (lines 3-6), along with the number of distinct values (line 7). Noise and flattening is computed from these statistics as with the algorithm of section [Operation of sum](#operation-of-sum).

Low-count suppression is done as with `sum()` ([Low count suppression](#low-count-suppression)).

# Aggregation function stddev

Currently `stddev(col)` is computed with per-UID information as done in Diffix Birch. See prior documentation for more information (https://arxiv.org/pdf/1806.02075.pdf).

# Classes of attacks

This section lists and describes the set of known attacks, and the mechanisms used by the cloak to defend against those attacks.

The primary goal of the cloak is to prevent attackers (analysts) from being able to determine attributes about individual users with high probability and high confidence. This must be the case even when the attacker has substantial prior knowledge about some or even most of the data in the database.

## Attribute value inspection attacks

Perhaps the most direct attack is to simply list column values. If the analyst can list a column value or set of column values that apply to a single user, then anonymity according to our goal would be violated.

A simple query for this attack would be:

```sql
SELECT ssn
FROM table
```

or for multiple columns, 

```sql
SELECT birthdate, zip, gender
FROM table
```

If the first query listed any social security numbers, then the privacy of the users with those numbers would be violated. Likewise if the second query listed users with a unique combination of birthdate, zipcode, and gender, then those users privacy would be violated according to our goal.

This attack is prevented through the use of *low-count suppression*.  Any column values that pertain to too few distinct users are suppressed by not being output.

## Suppression signal attacks

An attacker may be able to learn about individual users from whether a given bucket was suppressed or not. For instance, if a constant threshold were used for the suppress decision (i.e. suppress when 2 or fewer distinct users), then if the attacker happened to know that there is either 2 or 3 users with a certain set of attribute values, then if the bucket is not suppressed then the attacker knows that there must be three users, and potentially learns something about the third user.

To prevent this, the cloak uses a noisy threshold using sticky layered noise. Because the threshold itself can vary, in the previous example the attacker would be uncertain as to whether there were 2 or 3 users.

## Noise averaging attacks

Averaging attacks attempt to remove noise through multiple queries that somehow generate different noise samples (overcome the noise stickiness) and average away the noise.

### Naive averaging

The simplest averaging attack repeats the same query. This doesn't work because of sticky noise generates the same noise value.

### Different syntax but same semantics, with floating

Different noise samples are generated by queries that execute identical query semantics but with different syntax in the hope that the cloak does not recognize the same syntax and so produces a different noise sample.

A simple example would be two queries with the following `WHERE` clauses:

```
Q1: WHERE age = 10
Q2: WHERE age + 1 = 11
```

In principle, the cloak could interpret the math and determine that the above two expressions are identical. This can get extremely difficult, however, as the math gets more complex. Therefore for any expression that uses math, the cloak *floats* the column value by re-writing the SQL so that the column in question (here `age`) is brought up to the outer `SELECT` so that the actual selected values can be examined. 

However, other examples exist:

```
Q1: WHERE left(column,5)
Q2: WHERE substring(column FROM 1 FOR 5)
```

Not so many different semantically identical conditions can be made with string manipulation as with math manipulation, but nevertheless the above conditions would be floated to ensure that the seeds are identical.

### Different syntax but same semantics, without floating

The cloak doesn't always float. It does not float `col BETWEEN val1 AND val2` for instance. Rather, it deduces the seed material from SQL inspection. There are cases where same semantic but different syntax conditions can be generated from non-floated conditions. In a few cases, the cloak does not correctly account for the different syntax and uses different seeding material.

One such case is `BETWEEN` versus `bucket()`. For instance the range `100-200` can be generated with both of the following queries:

```
Q1: SELECT bucket(column BY 100) ....
Q2: WHERE column BETWEEN 100 and 200
```
The cloak, however, does not detect that these semantics are identical and generates different seeds as a result. Through this trick the analyst can get two noise samples for the layers associated with these conditions, and therefore reduce the overall noise somewhat.

In many other cases, however, the cloak does seed properly.

Text datatypes for instance are all converted to lower case for the purpose of seeding, so that the `lower()` and `upper()` functions can't be used to derive additional noise samples. 

### Split averaging attack

This attack creates pairs of queries where the sum of the queries in each pair have the same underlying value (i.e. before noise), but where each pair uses semantically different conditions from all other pairs, thus producing different noise values that can be averaged. This can be done with `WHERE` clauses of the following sort:

```
Pair 1:
Q1: WHERE column = 'X'
Q2: WHERE column <> 'X'
Pair 2:
Q1: WHERE column = 'Y'
Q2: WHERE column <> 'Y'
```

From pair 1, the first query Q1 matches all rows where `column = 'X'`, and Q2 matches all other columns (not equal to 'X'). The sum of Q1 and Q1 therefore includes all rows (that match other conditions, not shown here). The same holds for the two queries from pair 2. However, all of these conditions are semantically different and so produce different noise samples.

The cloak defends against this through *layered noise*. Each condition generates its own noise layers, which are summed together to produce the final noise value. The layers from the above `WHERE` clauses would indeed be averaged away, but the noise from other conditions in the queries would not average out. As a simple example, the following two queries are from one pair where the attacker goal is to learn the exact number of women:

Query 1:
```sql
SELECT count(DISTINCT uid)
FROM table
WHERE age = 20 and gender = 'F'
```

Query 2:
```sql
SELECT count(DISTINCT uid)
FROM table
WHERE age <> 20 and gender = 'F'
```

While the noise layers for `age` would indeed average out, the static noise layer for `gender = 'F'` would be the same across all queries in the attack and would result in a noisy final count.

### Linear program reconstruction

In this attack, the analyst generates queries where each query selects a different set of users, but where any given user is selected by a substantial number of queries. Repeatedly selecting among a pseudo-random subset of users would have this effect. The analyst then generates a set of equations and solves for the presence or absence of each user in that set of equations.

This attack was successfully run against Diffix Birch using the following WHERE clause:

```
WHERE floor(5 * pow((client_id * 2),0.5) + 0.5) = floor(5 * pow((client_id * 2), 0.5))
```

where different constants were used to effectly select different users.

The current defense is to force `clear` conditions (no math) on columns that have a substantial fraction of user-unique values.

## Difference attacks

In difference attacks, the attacker creates pairs of queries where the underlying true answers are either identical or differ by one user. The attacker then tries to determine which is the case.

A simple example would be the following pair of queries:

Query 1:
```sql
SELECT count(*)
FROM table
WHERE salary BETWEEN 100000 AND 110000 and
      ssn <> '539-54-9355'
```

Query 2:
```sql
SELECT count(*)
FROM table
WHERE salary BETWEEN 100000 AND 110000
```

The victim in this attack is the user with social security number (ssn) '539-54-9355'. The unknown attribute (the thing the attacker wants to learn) is whether or not the salary is in the range 100000 to 110000. Query 1 definitely excludes the victim, while Query 2 includes the victim if the victim has that salary range.

The simplest approach for the attacker is to deduce that the victim is in query 2 so long as `count(*)` is greater in query 2. Because of the noise, however, such an approach would often produce the wrong deduction.

The greater the difference between the two queries, however, the more likely the victim is indeed in that salary range. When the difference is large, the probability that the difference is due purely to the Gaussian distribution of noise is much less than the probability that the difference is due to both the noise distribution and the difference in the underlying true answer.

It is relatively rare, however, that the noise value is large enough to give the attacker a high-confidence deduction (like one in 10000 attacks).

### First derivative difference attack

There is a form of difference attack whereby the analyst generates a histogram of bucket pairs under the condition that the victim is not in the first query of each pair, and is in one and only one bucket of the second query. An example is the following:

Query 1:
```sql
SELECT bucket(salary BY 10000), count(*)
FROM table
WHERE ssn <> '539-54-9355'
```

Query 2:
```sql
SELECT bucket(salary BY 10000), count(*)
FROM table
```

If the noise layers were only static (based purely on the conditions themselves), then the difference in the noise between each pair of buckets would be the same except for the one containing the victim. This difference in the difference would identify the victim's salary.

The uid-based noise layers, however, add additional noise that is different between every pair of buckets.

### Difference attack with counting NULL

The effect of inserting [safe math functions](#determine-if-safe-math-functions-needed) is that column values may be NULL. This gives an attacker the opportunity to force a single user to have the only NULL value in the column. The attacker can determine if the NULL value is present by comparing the outputs of `count(*)` and `count(col)`. The former counts the NULL rows, the latter does not.

For example, the following difference attack can be used to learn the value of the `cli_district_id` of the user that has a transaction amount of 24615.

```
select cli_district_id, count(foo)
from (
	select uid, cli_district_id, 1/(amount - 24615) AS foo
	from transactions ) t
group by 1
```

```
select cli_district_id, count(*)
from (
	select uid, cli_district_id, 1/(amount - 24615) AS foo
	from transactions ) t
group by 1
```

The cloak defends against this by adding an additional UID noise layer for `count(col)`.

### Noise exploitation attacks

#### Through extreme user contribution

The magnitude of the noise must be in some sense proportional to the amount that the most extreme users contribute to an answer. For instance, suppose that one knows that roughly the average salary in a database excluding the CEO. By querying for the average salary, the amount by which it differs from the known average reveals the CEO's salary. The noise needs to be large enough to mask this salary. This is called *proportional noise*.

However, with a difference attack, the amount of noise itself can reveal the presence or absence of the CEO in a query. If the CEO has a substantially higher salary than anyone else, and if the query that may include the CEO has a lot of noise, then one can conclude that the CEO is present in the query.

To prevent this, the cloak removes a small number of the most extreme values in a given query (based on a noisy threshold), and then bases the amount of noise on the average value of a small group of users with the next most extreme values (also based on a noisy threshold). This is called *flattening*.

#### Through chaff conditions

In this difference attack, the attacker exploits the uid-based noise by intentionally adding conditions that have no impact on the true underlying answer, but increase the cumulative amount of noise. The bucket pair that differs the most is most likely the one containing the user.

A simple way to do this is to add conditions like `age <> 1000`, `age <> 1001` and so on. These are called chaff conditions. In pairs where the underlying set of users is the same, the uid-based noise values are the same for each query of the pair. In pairs where the underlying set of users is different, the uid-based noise value is different.

The chaff conditions can either all be added to the same query, or added one at a time to multiple pairs, and then summing the results across the first queries of each pair and separately the second queries.

### Range creep with averaging

One way to do a difference attack is to grow a range (`BETWEEN`) so that one additional user is included in the modified range. The noise layers associated with the range defeat a simple version of this attack that uses two queries. However, if an attacker could incrementally modify a range so that each change does not change the underlying answer, then the different noise values could be averaged. If the attacker averaged away the noise in this fashion for both the smaller and larger ranges, then the noise could no longer defend against the attack.

To defend against this, the cloak forces ranges to fall within preset range offsets and sizes. This makes it very unlikely that an attacker could get enough samples both excluding and including the victim.

## SQL backdoor attacks

A backdoor attack is where the attacker avoids defense mechanisms by encoding conditions indirectly through math. For instance, the following query:

```sql
SELECT count(*)
FROM table
WHERE age = 30 OR age = 40
```

Can be encoded in the following query without using a `WHERE` clause:

```sql
SELECT count(*), age 30 or 40 FROM (
    SELECT uid, (age 30 + age 40) % 2 as age 30 or 40
    FROM (
        SELECT uid,
            floor((age greater 29 + age less 31) / 2) AS age 30,
            floor((age greater 39 + age less 41) / 2) AS age 40
        FROM (
            SELECT uid,
                ceil((age - 29) / 100) AS age greater 29,
                ceil(0 - (age - 31) / 100) AS age less 31,
                ceil((age - 39) / 100) AS age greater 39,
                ceil(0 - (age - 41) / 100) AS age less 41
            FROM table
        ) x
    ) y
) z
GROUP BY age 30 or 40;
```

To prevent this, the cloak limits the amount of math, particularly non-continuous functions like `ceil` and `floor`, that can appear in a query.

## Side Channel attacks

### Error generation attacks

#### Divide by zero

Say that the victim's birthdate is known to be 14/12/1957, and zipcode is 60036.
The following query would trigger a divide-by-zero if the victim has a salary of 100000.

```sql
SELECT count(*) FROM
  (SELECT uid, z/(6003614.195712-z+d+y+m) FROM
     (SELECT uid, zipcode * 100 AS z,
             year(bday) / 10000 AS y,
             month(bday) / 1000000 AS m,
             day(bday) as d
      FROM user_info
      WHERE salary = 100000
     ) t1
  ) t2
```

Some databases throw and exception when divide-by-zero occurs. In these cases, the exception itself signals the salary of the victim.

#### Overflow

In some database, a numeric overflow throws an exception. This can be exploited, at least in Postgres, with for instance with the following attack.

```
select count(*)
from accounts
where lastname = 'Zamora' and
      birthdate = '1996-11-17' and
      2^(10000.01 * cli_district_id) = 123.12
```

The third condition in the `WHERE` clause causes an overflow if it is executed.  Assuming that there is only a single user with the lastname 'Zamora', if Zamora does not have the indicated birthdate, then the third condition won't be executed and a suppressed output is given. If Zamora does have that birthdate, then the third condition throws an exception. The exception is transmitted to the analyst as an execution error.

The cloak defends against this by installing and executing "safe" math routines in the database. The safe routines capture exceptions and returns NULL rather than throwing an exception. As a result there is no error signal transmitted to the analyst, and therefore the analyst doesn't know if an exception took place or not. See [Safe math functions](#safe-math-functions) and [Per column min and max values](#per-column-min-and-max-values).

Unfortunately the safe math routines slow down query execution. To minimize this performance hit, the cloak also makes a conservative estimate as to whether or not a given math expression *might* result in an exception. If not, then the safe math routine is not executed.


#### Square root of a negative number

### Timing attacks

#### JOIN timing attack

Because of optimizations in some database implementations, a query such as the following can be used in a timing attack:

```sql
SELECT count(*)
FROM (
  SELECT uid
  FROM accounts
  WHERE lastname = 'Zamora' AND birthdate = '1996-11-17' AND
        salary = 100000
) t1 JOIN (
  SELECT distinct uid
  FROM transactions
) t2 on t1.uid = t2.uid
```

The optimization here is that, if the left `JOIN` expression (`t1`) returns zero rows, then the right `JOIN` expression is either not executed or terminated before completion. Otherwise the right `JOIN` expression is executed to completion.

If the right `JOIN` expression takes a long time to compute, as this one does, then the analyst can determine whether or not the left `JOIN` expression has zero rows or not.

In the example above, the analyst may have know that there is only one user with name 'Zamora' and birthdate '1996-11-17'. If Zamora also has this salary, then the query execution time is shorter. If not, then the query execution time is longer.
[ghi3691](https://github.com/Aircloak/aircloak/issues/3691)
