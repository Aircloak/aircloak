
# Anonymization techniques

This document describes the methods used by the `anonymizer` module to compute aggregated values
in a privacy-preserving form. The final algorithm is a result of the discussion in
[#264](https://github.com/Aircloak/aircloak/issues/264).

The input to each method is the list of values for a bucket, grouped by user (so a list of lists).


## MAX() / MIN()

  - The maximum / minimum values per-user are taken and sorted in:
    - decreasing order for `MAX`.
    - increasing order for `MIN`.
  - The first value (user with the maximum / minimum value) is dropped.
  - Final result is the average value of the top Nc remaining users, where Nc is a noisy number with mean 5 and SD 1.
  - In case we don't have enough values available to compute the average, `null` is returned.


## COUNT()

  - The counts of values per-user are computed and sorted in descending order.
  - The first value (user with the biggest count) is dropped.
  - Compute the average count of the top Nc remaining users, where Nc is a noisy number with mean 5 and SD 1.
  - The total count is the sum of all the remaining counts plus Nv multiplied by the average count of the top,
    where NV is a noisy number with mean 1 and SD 2.
  - The final result is the maximum between the absolute lower bound of the LCF and the total count.


## SUM()

  - The sums of values per-user are computed and split into negative and positive values.
  - The final result is the anonymized sum of the positive values minus the anonymized sum of the negated negative values.
  - The anonymized sum of a set of positive values is computed as follows:
    - The values are sorted in descending order.
    - The first value (user with the biggest value) is dropped.
    - Compute the average value of the top Nc remaining users, where Nc is a noisy number with mean 5 and SD 1.
    - The total sum is the sum of all the remaining values plus Nv multiplied by the average count of the top,
      where NV is a noisy number with mean 1 and SD 2.


## AVG()

  - Returns the anonymized sum of values divided by the anonymized count of values.


## STDDEV()

  - The real average is computed.
  - For each value, the real variance is computed.
  - The final result is the anonymized average of the real variances.


## MEDIAN

  - The values are sorted in ascending order.
  - The real median is computed.  
  - The closest value per-user is extracted from above and below the median, from a noisy amount (mean: 5, sd: 1) of distinct users.
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

- We order values in descending order:

| values |
|--------|
| 10000 |
| 1000 |
| 1000 |
| 1000 |
| 1000 |
| 10 |
| 10 |

- We drop the first user:

| values |
|--------|
| 1000 |
| 1000 |
| 1000 |
| 1000 |
| 10 |
| 10 |

- We compute the noisy value for Nc: `Nc = 3`.
- We compute the average of the top Nc remaining users: `TopAverage = (1000 + 1000 + 1000) / 3 = 1000`.
- We compute the noisy value for Nv: `Nv = 1.3`.
- We compute the sum of all the remaining users: `Sum = 10 + 10 + 1000 + 1000 + 1000 + 1000 = 4020`.
- We compute the final result: `Result = Sum + Nv * TopAverage = 4020 + 1.3 * 1000 = 5320`.
