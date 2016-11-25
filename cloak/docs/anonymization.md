
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
