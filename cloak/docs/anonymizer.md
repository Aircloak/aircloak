
# Anonymization techniques

This document describes the methods used by the `anonymizer` module to compute aggregated values
in a privacy-preserving form. The final algorithm is a result of the discussion in
[#264](https://github.com/Aircloak/aircloak/issues/264).

The input to each method is the list of values for a bucket, grouped by user (so a list of lists).


## MAX() / MIN()

  - The maximum / minimum values per-user are taken and sorted in:
    - decreasing order for `MAX`.
    - increasing order for `MIN`.
  - The first value is dropped.
  - Final result is the average value of the noisy amount (mean: 5, sd: 1) of the top remaining values.
  - In case we don't have enough values available to compute the average, `null` is returned.


## COUNT()

  - The counts of values per-user are computed and sorted in descending order.
  - The user with the top count is removed.
  - The average count of the noisy amount (mean: 5, sd: 1) of the top remaining users is computed.
  - The total count is the sum of the remaining counts plus the noisy (mean: 1, sd: 2) average count of the top.
  - The final result is the maximum between the absolute lower bound of the LCF and the total count.


## SUM()

  - The sums of values per-user are computed and split into negative and positive values.
  - The final result is the anonymized sum of the positive values minus the anonymized sum of the negated negative values.
  - The anonymized sum of a set of positive values is computed as follows:
    - The values are sorted in descending order.
    - The user with the greatest value is removed.
    - The average sum of the noisy amount (mean: 5, sd: 1) of the top remaining users is computed.
    - The total sum is the sum of the remaining values plus the noisy (mean: 1, sd: 2) average sum of the top.


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
