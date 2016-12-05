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

  - Split the bucket into per-value-buckets on the given column
  - Order the per-value-buckets from smallest (least users) to biggest
  - Take a single row from each per-value-bucket
  - Feed this result into the appropriate anonymized aggregation function

  This last step ensures that the results for a query with `DISTINCT` do not
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
  - The first No users with the maximum / minimum overall values are dropped,
    where No is a noisy number with mean 4, SD 1 and lower bound 1.
  - Take the average value of the top Nt remaining users, where Nt is a noisy number with mean 5 and SD 1.
  - Final result is the maximum / minimum between the result of the previous step and
    the biggest / smallest value that would pass the low-count filter.
  - In case we don't have enough values available to compute the average, `null` is returned.


## COUNT()

  - The counts of values per-user are computed.
  - The first No users with the biggest overall counts are dropped,
    where No is a noisy number with mean 4, SD 1 and lower bound 1.
  - The average count of the top Nt remaining users is computed,
    where Nt is a noisy number with mean 5 and SD 1.
  - The total count is the sum of all the remaining counts plus No multiplied by
    the average count of the top Nt users plus Nv multiplied by the maximum value between
    the average count of the top Nt users and twice the average count of all the remaining users,
    where Nv is a noisy number with mean 0 and SD 1
    (`total = sum(remaining) + No * avg(top(remaining, Nt)) + Nv * max(avg(top(remaining, Nt)), 2 * avg(remaining))`).
  - The final result is the maximum between the absolute lower bound of the LCF and the total count.


## SUM()

  - The sums of values per-user are computed and split into negative and positive values.
  - The final result is the anonymized sum of the positive values minus the
    anonymized sum of the negated negative values.
  - The anonymized sum of a set of positive values is computed as follows:
    - The first No users with the biggest overall valuea are dropped,
      where No is a noisy number with mean 4, SD 1 and lower bound 1.
    - The average value of the top Nt remaining users is computed,
      where Nt is a noisy number with mean 5 and SD 1.
    - The total sum is the sum of all the remaining values plus No multiplied by
      the average value of the top Nt users plus Nv multiplied by the maximum value between
      the average value of the top Nt users and twice the average value of all the remaining users,
      where Nv is a noisy number with mean 0 and SD 1
      (`total = sum(remaining) + No * avg(top(remaining, Nt)) + Nv * max(avg(top(remaining, Nt)), 2 * avg(remaining))`).


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
    from a noisy amount (mean: 5, SD: 1) of distinct users on each side.
  - The final result is the average of the real median and the extracted values from above and below.
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

## Fixed alignment

This is part of a two-step process that protects users from being exposed by two
related queries that differ by a range applied to a column in the WHERE clause.
For the other part see [Shrink and drop](#shrink_and_drop).

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

This sets the stage for [Shrink and drop](#shrink_and_drop) since we limit the
possible queries to a discreet set.

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

## Shrink and drop

Given a query that scopes a certain column by an interval we need to check if
the analyst could have made another query with a slightly different interval
that differs by a `low_count` number of users. If so the rows outside of that
other query need to be suppressed from the output so that the result is the
same for both queries.

The perfect algorithm for this calls for analyzing all possible smaller ranges
for a given range and then recursively checking any data that remains after
suppression. Due to problems with implementing such an approach in a streaming
manner we decided on an approximate appraoch. The overall idea is to keep track
of a number of users with the highest and lowest values and buffer all records
of such users. At the end of the stream we decide how many of the most extreme
values need to be suppressed by producing an aligned range that encompasses most
of the data seen and rejecting rows that fall outside of it. Details:

* The input is a stream of rows.
* We buffer top and bottom N users with respect to the scoped column seen so far.
* When a row arrives we check if it's "safe" - that is the value for that row
is bigger than the biggest buffered value for the bottom N users and smaller
than the smallest buffered value for the top N.
* If it is then we emit it immediately.
* If it's not then we add it to the buffer. If the row belongs to a user that's
already in the buffer it is "attached" to that user. Otherwise a new entry for
the owner of that row is created. If the buffer exceeds size N at this point,
then all the rows from the "safest" (closest to the middle) user are removed
from the buffer and emitted.
* An exception to the above occurs when the user is in both the top N and bottom
N buffers - in that case the rows are "attached" in the other buffer instead
of being emitted.
* We collect the set of user ids from emitted rows.
* At the end of the stream we pick a random number with the same configuration as
the offset used for determining `low_count` using a seed generated from the set
of used ids.
* The number N above is picked in such a way that there is a very high probability of
it being bigger than the number chosen in the previous step so that we have sufficient
users to perform the calculation after we drop that many users.
* We skip that number of users from each side of the buffer. We find the smallest
range that encompasses all non-skipped entries in the buffer and align that range.
* We emit all buffered data that fits into the aligned range and suppress all data
that doesn't.
