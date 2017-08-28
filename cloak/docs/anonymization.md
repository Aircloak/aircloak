# Anonymization techniques

This document describes the algorithms we use to compute aggregates in a
privacy-preserving form. The final algorithm is a result of the discussion in
[#264](https://github.com/Aircloak/aircloak/issues/264).

The input to each method is the list of values for a bucket, grouped by user.

The hardcoded numbers used throughout this document can be changed during setup,
on a per-customer basis. For a specific cloak, the actual values are taken from
the [configuration file](../config/config.exs), in the `anonymizer` section.

## DISTINCT preprocessing

  (Based on https://github.com/Aircloak/aircloak/issues/324)

  Every function can be applied to a distinct version of the given column, for
  example `COUNT(DISTINCT name)` or `AVG(DISTINCT price)`. In those cases we
  apply the following preprocessing steps to a bucket of rows:

  * Input is a bucket split into per-user lists of values
  * Sort the bucket by the number of unique values in each list (ascending)
  * Flatten the result
  * Take each first unique value from this list
  * Group the list by user to produce a valid bucket again

  This is roughly equivalent to the statement in the issue above because the
  values in the flattened list will be picked first for users with a low number
  of values and only if they haven't appeared for any of those users they will
  be picked up with the id of a user with many unique values.

  This preprocessing ensures that the results for a query with `DISTINCT` do not
  vary greatly depending on the presence of just one user with many unique
  values in a given column, since the anonymizing aggregators will discard such
  outliers (see below).  Without that an attacker would be able to deduce if
  certain conditions are met for such a victim by comparing the results of two
  queries. For example if `SELECT COUNT(DISTINCT purchased_item) FROM foo`
  varies greatly from `SELECT COUNT(DISTINCT purchased_item) FROM foo WHERE
  credit_card_number = '00000000000'` and we know a person exists who purchased
  a lot of items nobody else purchased, then we now also know that person's
  credit card is `'00000000000'`.

  A downside of this approach is that we do not report an exact result even if
  we in principle could, because each distinct value has enough users. This
  could be rectified by adding a special case to the code, but for now we
  decided that uniform processing is more important, since we don't know how
  useful that case will be.


## MAX() / MIN()

  - The maximum / minimum values per-user are taken.
  - The first No users with the maximum / minimum overall values are dropped.
    No is calculated based on mean and SD values from [outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L41),
    using an absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
  - Take the average value of the top Nt remaining users.
    Nt is calculated based on mean and SD values from [top_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L48), using an
    absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
  - Add noise to the top average with mean 0 and SD equal a quarter of the standard deviation of the values
    used for the average calculation.
  - In case we don't have enough values available to compute the average, `null` is returned.
  - Note that a value might exist that appears frequent enough that the user could learn about it's existence through
    another query (SELECT COUNT(*) FROM table GROUP BY column) and that's larger than the value returned by this
    algorithm. We used to include an extra calculation that checked for the presence of such a value, however it seems
    that with the need to compute noise layers it would require keeping all the row data in memory at once, making it
    impossible to implement a streaming approach and significantly increasing memory requirements. Because of this we
    decided to not perform that extra step until a better solution can be found. See discussion in
    https://github.com/Aircloak/aircloak/pull/1430


## COUNT()

  - The counts of values per-user are computed.
  - The first No users with the biggest overall counts are dropped.
    No is calculated based on mean and SD values from [outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L41),
    using an absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
  - The average count of the top Nt remaining users is computed.
    Nt is calculated based on mean and SD values from [top_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L48), using an
    absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
  - The total count is the sum of all the remaining counts plus No multiplied by
    the average count of the top Nt users plus Nv multiplied by the maximum value between
    the average count of the top Nt users and twice the average count of all the remaining users,
    where Nv is a zero mean noisy number with a standard deviation taken from
    [sum_noise_sigma](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L51)
    (`total = sum(remaining) + No * avg(top(remaining, Nt)) + Nv * max(0.5 * avg(top(remaining, Nt)), avg(remaining))`).
  - Note that an `Nv * ...` factor is added _per noise layer_ - see [Noise Layers](#noise_layers).
  - Note that the No and Nt numbers are also calculated according to the rules in [Noise Layers](#noise_layers), so depending on the query the actual
    SD of these numbers might be bigger than configuerd.
  - The final result is the maximum between the absolute lower bound of the LCF and the total count.


## SUM()

  - The sums of values per-user are computed and split into negative and positive values.
  - The final result is the anonymized sum of the positive values minus the
    anonymized sum of the negated negative values.
  - The anonymized sum of a set of positive values is computed as follows:
    - The first No users with the biggest overall valuea are dropped.
      No is calculated based on mean and SD values from [outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L41),
      using an absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
    - The average value of the top Nt remaining users is computed.
      Nt is calculated based on mean and SD values from [top_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L48), using an
      absolute lower bound of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44).
    - The total sum is the sum of all the remaining values plus No multiplied by
      the average value of the top Nt users plus Nv multiplied by the maximum value between
      the average value of the top Nt users and twice the average value of all the remaining users,
      where Nv is a zero mean noisy number with a standard deviation taken from
      [sum_noise_sigma](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L51)
      (`total = sum(remaining) + No * avg(top(remaining, Nt)) + Nv * max(0.5 * avg(top(remaining, Nt)), avg(remaining))`).
    - Note that an `Nv * ...` factor is added _per noise layer_ - see [Noise Layers](#noise_layers).
    - Note that the No and Nt numbers are also calculated according to the rules in [Noise Layers](#noise_layers), so depending on the query the actual
      SD of these numbers might be bigger than configuerd.


## AVG()

  - Returns the anonymized sum of values divided by the anonymized count of values.


## STDDEV()

  - The real average is computed.
  - For each value, the square of the difference between the value and the real average is computed.
  - The anonymized average of the squared differences is computed.
  - The final output is the square root of the anonymized average.


## MEDIAN

  - The values are sorted in ascending order.
  - The real median is computed.
  - The closest value per-user is extracted from above and below the median,
    from a noisy amount (mean and SD taken from [top_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L48),
    with a minimum value of [min_outliers_count](https://github.com/Aircloak/aircloak/blob/master/cloak/config/config.exs#L44))
    of distinct users on each side.
  - The final result is the average of the real median and the extracted values from above and below,
    to which an additional noise component is added which has mean 0 and SD equal to a quarter of the
    standard deviantions of the values used for the median computation
  - In case we don't have enough values available to compute the average, `null` is returned.

## Example SUM() computation of positive values

- Input values (with total sum: 14020):

| values |
|--------|
| 10 |
| 500, 500 |
| 1000 |
| 2, 7 |
| 200, 300, 250, 250 |
| 1000 |
| 9000, 800, 200 |

- We sum values per-user:

| values |
|--------|
| 10 |
| 1000 |
| 1000 |
| 10 |
| 1000 |
| 1000 |
| 10000 |

- We compute the noisy value for No: `No = 1 + 2 = 3`.
- We drop the No users with the biggest values:

| values |
|--------|
| 10 |
| 10 |
| 1000 |
| 1000 |

- We compute the noisy value for Nt: `Nt = 3`.
- We compute the average of the top Nt remaining users: `TopAverage = (1000 + 1000 + 10) / 3 = 670`.
- We compute the noisy value for Nv: `Nv = 0.5`.
- We compute the sum of all the remaining users: `Sum = 10 + 10 + 1000 + 1000 = 2020`.
- We compute the noise scale: `NoiseScale = max(TopAverage, 2 * GlobalAverage) = max(670, 2 * 505) = 1010`
- We compute the final result: `Result = Sum + No * TopAverage + Nv * NoiseScale = 2020 + 3 * 670 + 0.5 * 1010 = 4535`.

## Low-count filtering

Some buckets are considered too small to report at all. These are ones that meet one of two conditions:

1. They are smaller than a hard low bound (configurable).
2. They are smaller than a random number chosen with a configurable mean and SD.

"Smaller" here means that the buckets contain less unique user ids than the given number. Note that the random
number is selected using the mechanism described in [Noise Layers](#noise_layers) so the configured SD is
only the base SD per layer - the total SD will depend on the exact query.

Data from all buckets discarded this way is aggregated into a single bucket and reported as a whole. Non-aggregated
values in this bucket are suppressed (replaced with a `*` in the output). This bucket is also checked for low-count
using the same procedure, and if it is found too small no data is reported.

## Fixed alignment

Whenever the query includes an inequality on some columns we require that there
is a complementary inequality bounding the variable from the other side. For
example `x > 10` and `x < 10 AND x > 20` are disallowed while `x > 10 AND x <= 20`
is allowed. We then align this range to a fixed grid with the following procedure:

* Find the closest size in the series `..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, ...`
  for which an aligned range of that size will contain the original range
* The low end of the range is the low end of the original range rounded down
  to the nearest half of the size
* The high end is the low end plus the size

Examples:

```
{1, 3} -> {1, 3}
{1, 4} -> {0, 5}
{3, 7} -> {2.5, 7.5}
{10.1, 11.9} -> {10.0, 12.0}
```

## Fixed alignment for dates

Date, time and datetime inequalities are also subject to fixed alignment. The
scheme used for the grid is a bit more complex to make the produced aligned
intervals feel more natural (like a quarter or a full month).

The interval is converted into units since epoch (or midnight for time intervals)
depending on its size - an interval of a couple minutes will use minutes,
while one that is about the length of a month will use months. That converted
interval is then aligned as described in [Fixed alignment](#fixed_alignment) with
the caveat that depending on the chosen grain (days, minutes, etc.) a different
grid sized is used. For example for minutes the allowed grid sizes are
`1, 2, 5, 15, 30, 60`. The result is converted back into a datetime interval.

## Noise Layers

This mechanism aims to defend against "difference attacks", whereby the analyst
uses the mere fact that two answers are different to conclude something about
the database. Before, the cloak defended against this by modifying answers so
that two queries that would otherwise produce different answers in fact produce
the same answer. Noise layers replace that with the opposite approach: they
ensure that two queries that would otherwise produce the same answer may well
in fact have different answers, so the analyst cannot be sure of the result of
his attack.

To achieve this all noisy values produced are generated using a set of random
number generators instead of just one. Each generator uses the same mean and SD
and the resulting noise is summed. The generators differ in their seed and how
exactly the seed is built depends on the query the analyst issued.

Specifically every filtering clause the analyst adds creates (at least) one
noise layer. The seed for that layer is based on the set of unique values in
the column that the filter is being applied on plus some additional data
depending on the filter. If multiple columns are used in a filter (for example
`a || b LIKE '%abc%'`), then a noise layer is created for each of those
columns. This way, even if two queries with different sets of filters would
have the same result they will have different noise applied, resulting in
(potentially) different results. See
[the original issue](https://github.com/Aircloak/aircloak/issues/1276) for more
details.

Note that the set of unique user ids is still used to generate one of the noise
layers, so even queries with no filters will have at least noise coming from
that layer.

### What's a filter?

The following things are considered filters:

* `WHERE` clauses
* Range `WHERE` clauses - a pair of `a >= x` and `a < y` is considered one
  filter instead of two
* `HAVING` clauses in subqueries
* `GROUP BY` clauses

### Noise layer seeds

Each noise layer is seeded with at least:

* A canonical name of the column in the form `{"table", "column"}`
* A secret salt
* The list of the values in the column
* A number N that is incremented each time two noise layers have exactly the
  same seed

In case the noise layers are used to compute a `COUNT(*)` as opposed to for
example a `COUNT(column)` expression an additional, unique marker is added to
the seed. This is meant to make it harder to exploit any knowledge about the
presence of `NULL` values in some column.

Additionally, depending on the type of clause, some extra data is added:

* `=` clauses, `LIKE` clauses, and `GROUP BY` - no extra data
* `<>` clauses and `NOT LIKE` clauses - a `:<>` symbol
* range clauses - the range endpoints
* `NOT IN` clauses - are converted to an equivalent conjunction on `<>` clauses,
  so noise layers are never directly computed for them
* `IN` clauses - a layer is created for the whole clause with no extra seed

### Floating data

The data for a noise layer is taken from the raw database column if possible.
However if the query aggregates it might not be possible to associate a unique
value of the column in question with every row. In that case 3 values are
produced instead - the min, max and count of the column. This process can be
repeated in case of multiple aggregations (in subqueries) by taking the min of
mins, max of maxes and sum of counts.

### Probing

In addition to a noise layer some clauses need to be checked with a probe. The
reason is that we don't have a way (yet?) to detect the "real" meaning of the
clause. For example both `a <> 4` and `sqrt(a) <> 2` do the exact same thing.
Instead of trying to guess what the effect would be from looking at the
expression alone we issue a probe to the database to judge the effect of the
clause on the query in practice.

Probing basically means that a helper query (probe) is issued to the database
for each "suspect" condition. The probe checks how many users the condition
excludes from the original query. If this number is low, then such a clause
and any corresponding noise layers are removed from the original query.

See [this noise layer issue](https://github.com/Aircloak/aircloak/issues/1768)
and [this probing issue](https://github.com/Aircloak/aircloak/issues/1777) for
more information about noise layers and probing.

### Low count checks

LIKE and ILIKE clauses are subject to another mechanism that hides some rows
matched by the query under certain conditions. This is because a LIKE is
implicitly an OR clause - for example the clause `x LIKE '_abc'` is the same as
`x = 'aabc' OR x = 'babc' OR ...`.

For every clause of the type `x LIKE y` where `x` might be some complex expression
`x` is floated to the top of the query. Then, before aggregation, we look at the
number of distinct values in `x`. If it's more than 4, then no rows are suppressed.
If it's less, then for each unique value we do a low-count computation, as usual.
The rows containing the values that are low-count are removed from the result
set before aggregation.

There are two additional complications:

1. For ILIKE, the floated values is a downcased version of what would be floated
   for LIKE to account for the case-insensitive nature of the clause.
2. If the rows are grouped in a subquery (e.g.
   `SELECT ... FROM (SELECT ... WHERE x LIKE y GROUP BY z) foo`) then we float
   `min` and `max` of the appropriate expression. These are then fed into the
   rules above, but if a value is found to be low-count then all _aggregated_
   rows containing that value in either `min` or `max` will be suppressed.
