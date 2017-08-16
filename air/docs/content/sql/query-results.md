# Understanding query results

`SELECT` queries return anonymized results. The results have noise added to them. This is crucial in protecting the privacy of individuals, while sufficiently unobtrusive to provide accurate results during normal use.

The results are anonymized in two phases:

1. Low-count filtering
2. Adding noise

## Low-count filtering

In this phase, values which are not associated with a sufficiently large number of distinct users are discarded. For example, consider the query `SELECT first_name FROM users`.

Let's say that the names in the `users` table are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 2
John   | 150
Mary   | 1
Tom    | 2

Since the number of distinct users named Bob, Mary, and Tom is too small, these names won't appear in the final result. In contrast, there is a sufficient number of Alices and Johns, so the result will contain the corresponding rows.

`*` rows are included in the result in place of rows that are discarded due to anonymization. In this example the distribution of rows after filtering would be:

Name   | Number of returned rows
------ | ------------------------
Alice  | 100
John   | 150
*      | 5

The number of `*` rows indicates the amount of properties that can't be included in the result. Note that this does not represent the number of _distinct_ omitted values. In this example, three distinct names are not reported (Bob, Mary, and Tom), but since there are two Bobs, one Mary, and two Toms, the result contains `2 + 1 + 2 = 5` `*` rows.

It is worth noting that absence of `*` rows doesn't mean that no rows were omitted. The `*` rows have to pass the same anonymization procedure. Thus, if the total count of `*` rows is too low, they will be omitted from the result.

## Adding noise

After low-count values are filtered, some amount of noise is introduced. Consider the example from the [previous section](#low-count-filtering), where there are 100 Alices, 150 Johns, and 5 other names. The final result might contain a slightly different distribution, for example 94 Alice rows, 152 John rows, and 7 `*` rows.

The results of aggregate functions, such as `SUM` and `COUNT`, are also anonymized. The returned values will slightly differ from the real values.

To ensure anonymity the amount of noise added depends on the number and types of filters used in the query. You might be able to get more accurate results by removing some `WHERE`- or `HAVING`-clauses from your query. Use the [avg_noise](#avgnoise), [count_noise](#countnoise), and [sum_noise](#sumnoise)  functions to get a better idea of how much noise is being added.

### `null` and aggregates of infrequently occurring values

Aircloak will report a value when the number of distinct users sharing the value exceeds a minimum threshold.

For example a query like

```SQL
SELECT name
FROM users
GROUP BY name
```

can safely return even infrequently occuring names.

The threshold for reporting a value, which is low (but safe), does under some circumstances not allow the system to produce anonymized aggregate values.
When this occurs `null` will be returned instead of an aggregate value. In the case of the `COUNT` aggregate the threshold value
is returned instead of `null` to remain compliant with standard SQL where `COUNT` is expected to return a non-null value.

As an example, let's consider a dataset containing 4 users with `name` Alice and an `age` column.
A query attempting to return aggregate properties of the `age` column will likely return a set of `null` values.

```SQL
SELECT
  name,
  count(*), count_noise(*),
  sum(age), sum_noise(age),
  avg(age), avg_noise(age),
  stddev(age), stddev_noise(age)
FROM users
GROUP BY name
```

Notice how `COUNT` still produces a non-`NULL` value. The reported count is not accurate but signifies an absolute lower
bound.

| name  | count | count_noise | sum  | sum_noise | avg  | avg_noise | stddev | stddev_noise |
|-------|-------|-------------|------|-----------|------|-----------|--------|--------------|
| Alice | 2     | null        | null | null      | null | null      | null   | null         |

## Probing

Some filtering clauses could indirectly reveal a user's identity. For example a clause like `name <> 'Fred'` could lead
to the analyst being able to deduce things about a person named `Fred` if that person is the only one with that name.

For that reason Aircloak needs to check how many unique users match certain clauses. In the example above Aircloak would
check how many users in a particular query have the name `Fred` and possibly remove the whole clause if that number is
too low. Because of that fact when two queries such as:

```sql
SELECT COUNT(*) FROM people WHERE name <> 'Fred'
```

and:

```sql
SELECT COUNT(*) FROM people
```

give the same output that doesn't mean there is nobody named `Fred` in the dataset. It might simply mean that there are too
few of them to report.

These checks are performed for all `<>`, `NOT LIKE`, and `NOT ILIKE` clauses.

## Anonymization functions

### avg_noise

```sql
AVG_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `AVG(...)` expression.

### count_noise

```sql
COUNT_NOISE(*)

COUNT_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `COUNT(...)` expression.

### sum_noise

```sql
SUM_NOISE(some_column)
```

Returns the standard deviation of the noise that would be added to an equivalent `SUM(...)` expression.
