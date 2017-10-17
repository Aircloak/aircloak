
# Understanding query results

All anonymization systems necessarily distort query results. While the amount of distortion in Aircloak is remarkably small, the analyst must nevertheless understand how and when distortion occurs to properly interpret query results.

This section describes how and when distortion occurs, and suggests strategies for minimizing the impact of distortion.

Aircloak distorts data in the following ways:

* Adds zero mean noise to anonymizing statistical function outputs
* May modify the values of "outlying" data
* May suppress certain results when too few users are represented (low-count filtering)

## Zero-mean noise

Aircloak adds zero-mean Gaussian noise to the outputs of `count`, `sum`, `avg`, and `stddev`. The "amount" (standard deviation, or "sigma") of the noise may vary. As a rule, the more that the values contributed by individual users vary between each other, the greater the noise.

For example, for a query counting distinct users (`count(DISTINCT uid)`), each user contributes at most 1, so the amount contributed by each user doesn't vary at all. Here the noise is quite small (typically sigma of less than 2 depending on the number of query conditions). On the other hand, a query that counts rows (`count(*)`) where some users have substantially more rows than other users will have more noise. The reason for this is to hide the effect of the highest contributing users and thereby protect their privacy.

Aircloak increases the noise with an increase in the number of certain query *conditions* (for instance those found in the `WHERE` and `HAVING` clauses). Specifically, each qualifying condition contributes at least two noise samples, which are then summed together. We refer to these noise samples as "noise layers". The following table gives the noise layers produced by each condition:

| Condition | Noise Layers |
| ----- | ----- |
| equality (`=` or `<>`) | Baseline (two noise layers) per column |
| Any `SELECT`'ed column | Baseline per column |
| `concat()` in equality | Baseline per column |
| range (`>=` and `<`, or `BETWEEN`) | Baseline |
| `IN` and `NOT IN` | Baseline plus one layer per `IN` component |
| `[I]LIKE` and `NOT [I]LIKE` | Baseline plus one layer per wildcard |
| None | One noise layer |

Aircloak provides functions that report the sigma of the zero-mean noise for each of the four functions. They are `count_noise()`, `sum_noise()`, `avg_noise()`, and `stddev_noise()` respectively. Note that the reported sigma are themselves rounded, but are generally within 5% of the true value.

### Examples

zzzz need to update these

**Example 1**

The answer to the following query indicates that noise with standard deviation of 1 was added to the count:

```sql
SELECT count(*), count_noise(*)
FROM accounts
``` 

count  | count_noise
------ | ------------------------
5370   | 1

This is because the `accounts` table has only one row per user, and therefore the amounts contributed by each user doesn't vary much.

**Example 2**

By contrast, for the following query, noise with standard deviation of roughly 340 was added:

```sql
SELECT count(*), count_noise(*)
FROM transactions
```

count   | count_noise
------  | ------------------------
1262872 | 320

The reason is that the number of transactions per user varies substantially in this table (the reported max is nearly 14000, the reported min is 5).

**Example 3**

The following query has noise with standard deviation of roughly 1.7:

```sql
SELECT count(*), count_noise(*)
FROM accounts
WHERE frequency = 'POPLATEK MESICNE' AND
      disp_type = 'OWNER'
```

count  | count_noise
------ | ------------------------
4167   | 1.700

This query has more noise than the query of Example 1 above because the two `=` conditions add additional noise layers.

** Example 4 **

zzzz

```sql
SELECT acct_date, count(*), count_noise(*)
FROM accounts
WHERE frequency LIKE 'P_PL_TE_ ME_I_NE' AND
      acct_district_id IN (30,31,33,35,37,39,41)
GROUP BY acct_date
```


## Low-count filtering

The Aircloak anonymizing statistical functions `count()`, `sum()`, `avg()`, `min()`, `max()`, `stddev()`, and `median()` compute outputs from the data of multiple users. If the number of distinct users used in an statistical function is too small, then the output is suppressed (not reported). This suppression is called "low-count filtering".

The threshold for the low-count filter is itself a noisy value with an average of four. If there are four distinct users that comprise an output, then there is a 50% chance the output will be suppressed. Fewer users increases the chance of suppression, and more users decreases the chance of suppression. Any reported output always has at least 2 distinct users. As a result, the value reported for `count()` is never less than 2.

For instance, suppose that a query counts the users with each given first name, and that the names in the `users` table are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 2
John   | 150
Mary   | 1
Tom    | 2


## Anonymizing statistical functions

Aircloak provides seven basic statistical functions that anonymize. They are: `count`, `sum`, `avg`, `stddev`, `min`, `max`, and `median`. These functions may add additional distortion besides the zero mean noise and low-count filtering already described. Note in particular that Aircloak gives no indication of whether any additional distortion occurred, or how severe this additional distortion is.

### sum()

The `sum()` function selects a small number of high values, and "flattens" them so that they are roughly the same. As a result, the users with high values appear as a group of users within which individual users can hide. The number of users chosen for flattening is itself a noisy value.

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

In any event, as a result of this flattening, the answer distortion in this particular extreme case is very large. The anonymized answer will be in the neighborhood of 12K where the true answer is over 1.5M. More generally, the amount of distortion depends on how much spread there is among the values of the highest (or in the case of negative values, lowest) users.

### count()

The `count()` function actually uses the `sum()` function, where the number of rows contributed by each user is the value being summed. As such, one or a small number of users with a high number of rows will be flattened.

Note that when counting distinct users, there is no added distortion.

### avg()

The `avg()` function is literally the result of the `sum()` function divided by the result of the `count(distinct uid)` function. As such, it also flattens the high (or negative low) users.

### stddev()

The `stddev()` function uses the `avg()` function, and so flattening occurs.

### max() and min()

The `max()` function drops the rows for a small number of users with the highest values (using a noisy number of users as with `sum()`). It then takes the average value of the next small number of distinct users with the highest values, and uses this average as the max (with potentially some additional noise, if the spread among this set of values is high). As such, the anonymized max may be very far from the true max.

The `min()` function operates the same as `max()`, except using low numbers. Unless the data includes negative numbers, `min()` tends to have less distortion than `max()`.

### median()

The `median()` function uses the average value of those of a small number of distinct users with values above and below the true median (with potentially some additional noise, if the spread among this set of values is high). In practice, the `median()` function usually does not introduce much distortion.

### When flattening cannot occur

The above anonymizing statistical functions must select a certain number of distinct users for the flattening procedure. It can easily happen that there are enough distinct users that the low-count filter is passed (no suppression occurs), but still not enough distinct users for the flattening procedure.

In these cases, the row will be reported, but a default value will be given as the output of the anonymizing statistical function. If the function is `count()`, then the default value is `2`. For all other functions, the default value is `null`. 

The reason that `2` is used for `count()` instead of `null` is because by the rules of SQL, `count()` must return an integer. The reason that `2` is used instead of `0` for `count()` is that in any event every released count has at least 2 distinct users.


---------------------------

![Shows an overview of different ways in which the Aircloak Insights components interact. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](components/interactions.png)



