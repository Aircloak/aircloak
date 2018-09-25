# Best practises

Because Aircloak Insights anonymises query results, it must be queried in a slightly different way
than one would query a normal database. This guide will explain some of the peculiarities and show
best practises which allow you to gain the most value from the system.

In the following examples we will pretend we are querying a database containing the following
raw data:

| first_name | last_name | age | zip_code | gender |
|------------|-----------|-----|----------|--------|
| Alice      | Anderson  | 10  | 10000    | F      |
| Amanda     | Anderson  | 20  | 11000    | F      |
| Amy        | Anderson  | 15  | 11000    | F      |
| Anna       | Anderson  | 20  | 11000    | F      |
| Bob        | Anderson  | 10  | 10000    | M      |
| Bob        | Barlow    | 10  | 10000    | M      |
| Bob        | Boyle     | 10  | 10000    | M      |
| Bob        | Buckner   | 10  | 10000    | M      |

## How anonymisation alters results

Aircloak Insights' anonymisation does not change the values in the columns you select.
Instead, it alters the aggregates resulting from your queries.

In other words, if you were to write the query:

```sql
SELECT zip_code, gender, count(*)
FROM table
GROUP BY zip_code, gender
```

you would be given the exact zip code and gender values as they appear in the database.
The anonymized result might look something like this:

| zip_code | gender | count |
|----------|--------|-------|
| 11000    | F      | 3     |
| 10000    | M      | 4     |

What is anonymised is the aggregate - in this case the count. Instead of getting a count of 4
for the zip code and gender pair 10000 and M you might see a count between 2 and 6.

Aircloak Insights also filters out values that do not appear frequently enough. We call this low count filtering.
In the case of the query above we see that the only female living in zip code 10000 is Alice.
Instead of giving us an altered count, the system will withold this information altogether.

More details about how the aggregates get altered can be found in the [understanding query
results](query-results.md) chapter of these user guides.

## Column selection and its effect on anonymisation

When querying a traditional database system it is common to run a query such as `SELECT *` or
otherwise select a wide range of columns. This does not work well with the approach Aircloak
Insights takes to anonymisation.

Aircloak Insights filters out rows from the result set where the combination of values in the selected columns
do not appear for enough distinct individuals in the dataset. Aircloak Insights could give you information
about males living in zip code 10000 (there are 4 of them), but not about the sole female living in the
same zip code.

Running the query `SELECT * FROM table` is, in the context of the dataset above, the same as writing the query:

```sql
SELECT first_name, last_name, age, zip_code, gender
FROM table
```

If we look at the example data, we see that the combination of these attributes is always unique.
For example while there are multiple Bob's, they all have distinct last names,
and while there are quite a number of individuals with the last name Anderson, none of them share
the same first name. Because of these unique combinations of columns, effectively no results can be shown,
and, as a general rule, the more columns you select in a query, the lower the probability that
there is data from sufficiently many distinct users that share the same attribute values and thereby
won't get anonymised away.

Aircloak Insights will attempt to provide some value even in the cases where you select many columns
while there is not enough unique individuals to provide values for the full set of column combinations.
It will do so by selectively replacing one column at a time with a `*` (indicating that the value was anonymised away).
This process takes place from the rightmost to the leftmost selected column. For the following examples we will assume
the minimum threshold for the number of individuals needed to pass the low count filter is 3. The reality is somewhat
more complex, but it's enough for the purposes of this example.
In the example tables below we will add a metadata column as the first column showing how many
unique users share a set of column values. The initial value of 1 for each row
indicates that each combination of column values only exists once:

| # individuals |  first_name | last_name | age | zip_code | gender |
|---------------| ------------|-----------|-----|----------|--------|
| 1             |  Alice      | Anderson  | 10  | 10000    | F      |
| 1             |  Amanda     | Anderson  | 20  | 11000    | F      |
| 1             |  Amy        | Anderson  | 15  | 11000    | F      |
| 1             |  Anna       | Anderson  | 20  | 11000    | F      |
| 1             |  Bob        | Anderson  | 10  | 10000    | M      |
| 1             |  Bob        | Barlow    | 10  | 10000    | M      |
| 1             |  Bob        | Boyle     | 10  | 10000    | M      |
| 1             |  Bob        | Buckner   | 10  | 10000    | M      |

The rightmost column is dropped first. In this case this is the gender column since it was the
rightmost selected column in the query:

| # individuals |  first_name | last_name | age | zip_code | gender |
|---------------| ------------|-----------|-----|----------|--------|
| 1             |  Alice      | Anderson  | 10  | 10000    | *      |
| 1             |  Amanda     | Anderson  | 20  | 11000    | *      |
| 1             |  Amy        | Anderson  | 15  | 11000    | *      |
| 1             |  Anna       | Anderson  | 20  | 11000    | *      |
| 1             |  Bob        | Anderson  | 10  | 10000    | *      |
| 1             |  Bob        | Barlow    | 10  | 10000    | *      |
| 1             |  Bob        | Boyle     | 10  | 10000    | *      |
| 1             |  Bob        | Buckner   | 10  | 10000    | *      |

Each set of column value combinations is still uniquely identifying a user, so
the next rightmost column gets replaced. In this case that is the zip code:

| # individuals |  first_name | last_name | age | zip_code | gender |
|---------------| ------------|-----------|-----|----------|--------|
| 1             |  Alice      | Anderson  | 10  | *        | *      |
| 1             |  Amanda     | Anderson  | 20  | *        | *      |
| 1             |  Amy        | Anderson  | 15  | *        | *      |
| 1             |  Anna       | Anderson  | 20  | *        | *      |
| 1             |  Bob        | Anderson  | 10  | *        | *      |
| 1             |  Bob        | Barlow    | 10  | *        | *      |
| 1             |  Bob        | Boyle     | 10  | *        | *      |
| 1             |  Bob        | Buckner   | 10  | *        | *      |

The zip code column is then followed by the age column:

| # individuals |  first_name | last_name | age | zip_code | gender |
|---------------| ------------|-----------|-----|----------|--------|
| 1             |  Alice      | Anderson  | *   | *        | *      |
| 1             |  Amanda     | Anderson  | *   | *        | *      |
| 1             |  Amy        | Anderson  | *   | *        | *      |
| 1             |  Anna       | Anderson  | *   | *        | *      |
| 1             |  Bob        | Anderson  | *   | *        | *      |
| 1             |  Bob        | Barlow    | *   | *        | *      |
| 1             |  Bob        | Boyle     | *   | *        | *      |
| 1             |  Bob        | Buckner   | *   | *        | *      |

Even replacing the age column does not produce rows with column values that wouldn't be
uniquely identifying. The next row to be taken away is the last name column:

| # individuals | first_name | last_name | age | zip_code | gender |
|---------------|------------|-----------|-----|----------|--------|
| 1             | Alice      | *         | *   | *        | *      |
| 1             | Amanda     | *         | *   | *        | *      |
| 1             | Amy        | *         | *   | *        | *      |
| 1             | Anna       | *         | *   | *        | *      |
| 4             | Bob        | *         | *   | *        | *      |

When taking away the last name column, we see that there are sufficiently many individuals named Bob
that it can be reported. For the remaining rows Aircloak Insights will then also try to take away
the first name column, to at least give an indication of how many rows had to be filtered away.
That leaves us with the final table:

| # individuals | first_name | last_name | age | zip_code | gender |
|---------------|------------|-----------|-----|----------|--------|
| 4             | *          | *         | *   | *        | *      |
| 4             | Bob        | *         | *   | *        | *      |

We can take away a number of things from this example:

1. The higher the number of distinct colums you select in your query, the lower the likelihood that
   the set of individual attributes occur frequently enough to pass the low count threshold.
1. Queries such as `SELECT * ...` will usually not yield useful information in the context of Aircloak Insights.
1. As an analyst you can directly influence the order in which Aircloak Insights will drop columns.
   The process takes place from the right to left. Therefore you should order the columns from most to least important.
1. Selecting a column that has a unique value per user, or close to it,
   as one of your first columns will automatically lead to all the columns right of it being
   anonymised away. You are therefore likely better off selecting columns with a high number
   of distinct values as one of your later columns as chances are they will get dropped.

Let us now revisit the same query again, but looking at what the result would have
been if we changed the order in which we selected the columns.
Let's say the query we ran was:

```sql
SELECT gender, zip_code, first_name, age, last_name
FROM table
```

Just like last time none of the rows occur frequently enough to pass the anonymiser,
and Aircloak Insights will drop the rightmost column, in this case last name:

| # individuals | gender | zip_code | first_name | age | last_name |
|---------------|--------|----------|------------|-----|-----------|
| 1             | F      | 10000    | Alice      | 10  | *         |
| 1             | F      | 11000    | Amanda     | 20  | *         |
| 1             | F      | 11000    | Amy        | 15  | *         |
| 1             | F      | 11000    | Anna       | 20  | *         |
| 4             | M      | 10000    | Bob        | 10  | *         |

We already have a value that can be reported, namely that there are 4 male Bob's living in zip code 10000 aged 10.

There are still a set of other rows that need further refining.
Aircloak Insights attempts to drop the next rightmost column, namely age:

| # individuals | gender | zip_code | first_name | age | last_name |
|---------------|--------|----------|------------|-----|-----------|
| 1             | F      | 10000    | Alice      | *   | *         |
| 1             | F      | 11000    | Amanda     | *   | *         |
| 1             | F      | 11000    | Amy        | *   | *         |
| 1             | F      | 11000    | Anna       | *   | *         |
| 4             | M      | 10000    | Bob        | 10  | *         |

Dropping the age column did not make any of the other rows reach the low count
threshold, so the next rightmost column is dropped: first name.

| # individuals | gender | zip_code | first_name | age | last_name |
|---------------|--------|----------|------------|-----|-----------|
| 1             | F      | 10000    | *          | *   | *         |
| 3             | F      | 11000    | *          | *   | *         |
| 4             | M      | 10000    | Bob        | 10  | *         |

As a result of dropping the first name we now see that we can additionally report
that there are 3 females living in zip code 11000.

After removing the first name column there are too few individuals remaining
to produce further reportable rows, so the process is finished with the
following result:

| # individuals | gender | zip_code | first_name | age | last_name |
|---------------|--------|----------|------------|-----|-----------|
| 3             | F      | 11000    | *          | *   | *         |
| 4             | M      | 10000    | Bob        | 10  | *         |

Which is quite a bit more informative than what we got previously:

| # individuals | first_name | last_name | age | zip_code | gender |
|---------------|------------|-----------|-----|----------|--------|
| 4             | *          | *         | *   | *        | *      |
| 4             | Bob        | *         | *   | *        | *      |

## Grouping values

The problem shown above of there needing to be enough distinct users with a given set of column values
gets exasperated when one of the columns contains values that are prone to have a large spread like
integer values, or that are likely to be unique like floating point values or high resolution date and time values.

As an analyst you have more knowledge about what your desired outcome is than Aircloak Insights does.
You might, for example, know that age groups spanning 10 years would be perfectly fine,
that you only care about the first digit of a zip code or a salary amount rounded to the nearest thousand.

These types of explicit groupings of numerical values make it much more likely that values will get past the low
count filter, and can be achieved by using the `bucket` function. The bucket function is described in more detail
in the [Supported functions](functions.md#mathematical-functions) chapter, but suffice to say that
`bucket(age by 10)` would group the age values to the nearest 10. By default values are rounded down,
such that 11, 15, as well as 19 all become 10, while 20, 21, 25 all become 20, etc.

In order to achieve our objective of getting ages by the nearest 10, and zip codes by the first digit,
we could run the query:

```sql
SELECT
  bucket(age by 10) as age_bucket,
  bucket(zip_code by 10000) as zip_code_bucket
FROM table
```

Which would result in the following table of values:

| # individuals | age | zip_code |
|---------------|-----|----------|
| 6             | 10  | 10000    |
| 2             | 20  | 10000    |

which after low count filtering, would become:

| # individuals | age | zip_code |
|---------------|-----|----------|
| 6             | 10  | 10000    |

where the age group 20 to 30 got removed because there were not enough individualas within the given
zip code group.

You can play similar tricks with times, dates, and datetimes by using the functions `year`, `month`,
`day`, `hour`, `minute`, and `second` to extract components from the date or time, or alternatively use
`date_trunc(part, dateTimeColumn)` which truncates everything going beyond a certain level of accuracy.
For example `date_trunc('hour', '12:22:44.004200')` would turn the time into one at hour resolution: `12:00:00.000000`.
This value is more likely to pass the low count filter than the high resolutiom time value would be.

## null values and counts of 2

In most dialects of SQL all but the `count` aggregate may produce a `null` value. The `count` aggregate would, lacking
data to produce a count return 0 rather than a `null`.

Aircloak Insights will return a `null` value if there are insuficient data to produce a properly anonymized aggregate,
but there were enough data for a result to pass the low count filter.

Say we ran the query:

```sql
SELECT last_name, avg(age)
FROM table
GROUP BY last_name
```

| last_name | avg  |
|-----------|------|
| Anderson  | null |
| *         | null |

The low count filter would filter out all last names other than Anderson. In the case of the 5 Andersons we have enough users
to inform you as an analyst that users with the last name of Anderson exist in the dataset, but not enough to make a properly
anomymized average age. The `avg(age)` would therefore be returned as `null`.

In the case of `count` we might have enough distinct users to produce a count for the number of Anderson's, but not enough
other users to generate a count for the `*` row. Unlike for `avg` aggregate Aircloak Insights cannot report a `null` value
as that would be incompatible with most existing tools. Instead the system will return a placeholder value of 2, which is a
hardcoded value in the system. The presence of 2 in a `count` should therefore be considered
as information about the fact that there are users with the given properties in the dataset, but not enough to produce
a proper count. To validate that this is what is going on, you could also request the matching `count_noise()` value, which
in this case would be `null`.

If we ran the query:

```sql
SELECT last_name, count(*), count_noise(*)
FROM table
GROUP BY last_name
```

We can validate that the count of 2 for the `*` row is due to there being insufficiently many users. The `count_noise`
being `null` confirms this.

| last_name | count | count_noise |
|-----------|-------|-------------|
| Anderson  | 5     | 1.4         |
| *         | 2     | null        |


If you want to hide these `2` values from your result set you can add a `HAVING` clause to your query like this:

```sql
SELECT last_name, count(*)
FROM players
GROUP BY last_name
HAVING count(*) > 2
```

| last_name | count |
|-----------|-------|
| Anderson  | 5     |

## Implicit aggregate count

If you write a query that does not contain an explicit aggregate, for example:

```sql
SELECT last_name
FROM table
```

then Aircloak Insights will internally run this query as if you had included a `count(*)`, namely the query:

```sql
SELECT last_name, count(*)
FROM table
GROUP BY last_name
```

Each occurrence of `last_name` would then be repeated `count(*)` number of times.
For example the results of this query given the example dataset we have been working with in this guide would be:


| last_name | count |
|-----------|-------|
| Anderson  | 5     |
| *         | 3     |

which then would result in the following table being returned to you as an analyst:

| last_name |
|-----------|
| Anderson  |
| Anderson  |
| Anderson  |
| Anderson  |
| Anderson  |
| *         |
| *         |
| *         |

When running these types of queries you might end up seeing a lot of rows appearing exactly two times.
This is an artifact of what was described above, namely that the implicit `count(*)`
added by Aircloak Insights returns 2 as a placeholder for the value `null` as a means of indicating
that the value did not appear frequently enough to produce a properly anonymized aggregate.
