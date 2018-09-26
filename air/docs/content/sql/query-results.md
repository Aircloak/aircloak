# Understanding query results

All anonymisation systems necessarily distort query results. While the amount of distortion in Aircloak is remarkably small, the analyst must nevertheless understand how and when distortion occurs to properly interpret query results.

This section describes how and when distortion occurs, and suggests strategies for minimizing the impact of distortion.

Aircloak distorts data in the following ways:

* Adds zero mean noise to anonymising aggregation function outputs
* May modify the values of *outlying* data (the highest or lowest values in a column)
* May suppress certain results when too few users are represented (low-count filtering)

## Pro Tips

* Use the noise reporting functions (e.g. `count_noise()`) to determine how much noise is added.
* Remember that outlying values are *flattened*: sums, row counts, and maxes may be quite inaccurate when users with extreme outlying values are in the data.
* Look for the `*` output row to gauge how much data is being hidden.
* Output rows with `NULL` probably mean that there are not enough distinct users to compute an anonymised aggregate.
* In the Aircloak web interface, output rows in italics have more relative distortion.
* Queries with fewer and simpler conditions have less noise.

## Zero-mean noise

Aircloak adds zero-mean Gaussian noise to the outputs of `count`, `sum`, `avg`, and `stddev`. The *amount* (standard deviation, or *sigma*) of the noise may vary. As a rule, the noise is roughly proportional to the influence that the most influential users have on the output. For example:

* If the sum of salaries is being computed, and the highest salaries are around $1,000,000, then the sigma will be proportional to $1,000,000.
* If the count of distinct users is being computed, then the sigma will be proportional to 1 (the maximum amount that any user contributes to the count).

The reason for this is to hide the effect of the highest contributing users and thereby protect their privacy.

Aircloak increases the noise with an increase in the number of certain query *conditions* (for instance those found in the `WHERE` and `HAVING` clauses). Specifically, most conditions contribute a *baseline* of two noise samples, and some conditions contribute additional samples. These noise samples are summed together.  We refer to the noise samples as *noise layers*. The following table gives the noise layers produced by each condition:

| Condition | Noise Layers |
| ----- | ----- |
| equality (`=` or `<>`) | Baseline (two noise layers) |
| Any `SELECT`'ed column | Baseline |
| `concat()` in equality | Baseline |
| range (`>=` and `<`, or `BETWEEN`) | Baseline |
| `IN` | One layer plus one layer per `IN` element |
| `NOT IN` | Two layers per `NOT IN` element |
| `[I]LIKE` and `NOT [I]LIKE` | One layer plus one layer per wildcard |
| `right`, `left`, `ltrim`, `rtrim`, `btrim`, `trim`, or `substring` | Baseline plus one layer |
| `upper`, `lower` with `<>` | Baseline plus one layer |
| `col1 <> col2` (special case of `<>`) | No noise layer |
| None | One noise layer |


Aircloak provides functions that report the sigma of the zero-mean noise for `count()`, `sum()`, `avg()`, and `stddev()`. They are `count_noise()`, `sum_noise()`, `avg_noise()`, and `stddev_noise()` respectively. Note that the reported sigma are themselves rounded, but are generally within 5% of the true value.

### Examples

**Example 1**

The answer to the following query indicates that noise with `sigma = 2` was added to the count:

```sql
SELECT count(*), count_noise(*)
FROM accounts
```

count  | count_noise
------ | ------------------------
5368   | 2

This is because the `accounts` table has only one row per user, and therefore the amount contributed by the most influential user is just 1.

**Example 2**

By contrast, for the following query, noise with sigma of roughly 340 was added:

```sql
SELECT count(*), count_noise(*)
FROM transactions
```

count   | count_noise
------  | ------------------------
1262167 | 320

The reason is that the number of transactions per user varies substantially in this table (the reported max is nearly 14000, the reported min is 5).

**Example 3**

The following query has noise with sigma of roughly 2:

```sql
SELECT count(*), count_noise(*)
FROM accounts
WHERE frequency = 'POPLATEK MESICNE' AND
      disp_type = 'OWNER'
```

count  | count_noise
------ | ------------------------
4167   | 4

This query has more noise than the query of example 1 above because each of the two conditions adds two noise layers. Each layer has `sigma = 2`, so the resulting cumulative sigma is `sqrt(4) * 2 = 4`.

**Example 4**

The following query produces answer rows with `sigma = 4`. This represents 16 noise layers: 2 for `acct_date`, 6 for the `LIKE` condition, and 8 for the `IN` condition.

```sql
SELECT acct_date, count(*), count_noise(*)
FROM accounts
WHERE frequency LIKE 'P_PL_TE_ ME_I_NE' AND
      acct_district_id IN (30,31,33,35,37,39,41)
GROUP BY acct_date
```


## Low-count filtering

The Aircloak anonymising aggregator computes outputs from the data of multiple users. If the number of distinct users contributing to a single output row is too small, the row is suppressed (not reported). This suppression is called *low-count filtering*.

The threshold for the low-count filter is itself a noisy value with an average of 4. If there are 4 distinct users that comprise an output row, then there is a 50% chance the row will be suppressed. Fewer users increases the chance of suppression, and more users decreases the chance of suppression. Any reported output row always has at least 2 distinct users.

For instance, suppose that a query counts the users with each given first name, and that the names in the `users` table (before anonymisation) are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 2
John   | 150
Mary   | 1
Tom    | 2

Since there is only one Mary, she definitely won't appear in the output. Since there are only two Bobs and Toms, their names probably won't appear in the output. Therefore, the anonymised result returned by Aircloak may be something like:

Name  |	Number of distinct users
----  | ------------------------
Alice |	102
John  |	147
`*`   |	7

The `*` row provides the analyst with an indication that some names have been suppressed because of low-count filtering. This indication is particularly important in cases where a large number of values are low-count filtered: the analyst can learn that a substantial amount of data is being hidden. Note that the `*` row is itself anonymised: the anonymised aggregate associated with it has noise, and it itself is low-count filtered. In other words, lack of a `*` row does not mean that no data was suppressed, only that very little data was suppressed.

When a large number of non-aggregated columns is selected in a query, the chances of having lots of rows with very
few users increase. That will lead to lots of rows being suppressed, making the query result less useful.
In order to suppress as little information as possible, Aircloak will low-count filter columns individually, from
right to left. Rows that are suppressed in one iteration are aggregated together and kept for the next round of
filtering. That way, the maximum number of rows will be sent back to the analysts.

If the following query is issued:

```sql
SELECT name, age, COUNT(DISTINCT uid)
FROM table
GROUP BY name, age
```

and the non-anonymised results are:

| name | age | count | sufficient users |
|------|-----|--------|-----------------|
| Alice | 10 | 2 | false |
| Alice | 20 | 2 | false |
| Bob | 30 | 1 | false |
| Cynthia | 40 | 2 | false |

and the system only allows through values where there 3 or more distinct users in the answer set, then the Insights
Cloak will attempt to group the low-count values together by the `age` column, and, where necessary, also by the
`name` column, as follows:

Step 1: Suppress `age` where necessary

| name | age | count | sufficient users |
|------|-----|--------|-----------------|
| Alice | * | 4 | true |
| Bob | * | 1 | false |
| Cynthia | * | 2 | false |

Step 2: Suppress `name` where necessary

| name | age | count | sufficient users |
|------|-----|--------|-----------------|
| Alice | * | 4 | true |
| * | * | 3 | true |

This process is time-consuming, so it is limited by default to a maximum of 3 columns. For details on how to change this
limit, refer to the [Configuring the Insights Cloak](../ops/configuration.md#insights-cloak-configuration) section. A value
of 1 results in a single bucket for suppressed data, while a value of 0 will drop the low-count filtered data completely.

## Anonymising aggregation functions

These seven anonymising aggregation functions may add additional distortion besides the zero mean noise and low-count filtering already described. Note in particular that Aircloak gives no indication of whether any additional distortion occurred, or how severe this additional distortion is. This is because such information itself may leak individual information.

Anonymising aggregation functions make a variety of computations that require some minimum number of distinct users. It can happen that there are enough distinct users to pass a low-count filter, but not enough distinct users to compute the aggregate. In these cases, Aircloak does not suppress the output, but rather reports `NULL` for all aggregation functions and noise reporting functions except `count()`. Aircloak reports 0 for `count()` because `NULL` is not a valid output for `count()`.

Note also that when there are fewer than 15 distinct users (anonymised output) in a given output row, then the Aircloak web interfaces reports the output in italics. This serves as a reminder to the analyst that the result likely has high relative noise.

### sum()

The `sum()` function selects a small number of the highest values, and *flattens* them so that they are roughly the same. In other words, a few high values are lowered, or in the case of negative numbers, a few low values are increased. As a result, the users with high values become a homogeneous group of users within which individual users can hide. The number of users chosen for flattening is itself a noisy value.

By way of example, suppose that values in a given summed column contain the following numbers of distinct users:

Value   | Number of distinct users
------ | ------------------------
1  | 1000
500    | 20
500K | 1
1M | 1

Aircloak will flatten the high values by modifying them to fall within a group of high value users. In this example, the high value group has the value 500, and so the users with values 500K and 1M are replaced with 500.  The resulting values are these:

Value   | Number of distinct users
------ | ------------------------
1  | 1000
500    | 22

The users with 500K and 1M have, essentially, disappeared from the system. The affect is similar to outlier removal in statistics, and the analyst needs to be aware that this is happening, and that there is no indication from Aircloak that it has happened.

In addition, the sigma of the noise is proportional to the average value of the modified group of high users (in this case 500). This can be observed from the `sum_noise()` function.

In any event, as a result of this flattening, the answer distortion in this particular extreme case is very large. The anonymised answer will be in the neighborhood of 12K where the true answer is over 1.5M. More generally, the amount of distortion depends on how big the outlying values are relative to other values.

### count()

The `count()` function actually uses the `sum()` function, where the number of rows contributed by each user is the value being summed. As such, one or a small number of users with a high number of rows will be flattened.

Note that when counting distinct users, there is no added distortion.

### avg()

The `avg(col)` function is literally the result of the `sum(col)` function divided by the result of the `count(col)` function. As such, it also flattens the high (or negative low) users.

### stddev()

The `stddev()` function uses the `avg()` function, and so flattening occurs.

### max() and min()

The `max()` function drops the rows for a small number of users with the highest values (using a noisy number of users as with `sum()`). It then takes the average value of the next small number of distinct users with the highest values, and uses this average as the max (with potentially some additional noise, if the spread among this set of values is high). As such, the anonymised max may be very far from the true max.

The `min()` function operates the same as `max()`, except using low numbers. Unless the data includes negative numbers, `min()` tends to have less distortion than `max()`.

### median()

The `median()` function uses the average value of those of a small number of distinct users with values above and below the true median (with potentially some additional noise, if the spread among this set of values is high). In practice, the `median()` function only introduces significant distortion when values vary substantially around the true median.
