# Aircloak Query Language

To write queries, you can use Aircloak Query Language (AQL). AQL is a subset of standard SQL, implemented to prevent leakage of sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain anonymized data from tables. See [Understanding query results](understanding-query-results) for an explanation of the effects of anonymization on the results.

The syntax conforms to the standard SQL syntax, but only a subset of features is supported. The general shape of the query looks like:

<pre style="float:left; background-color:inherit; color:inherit; text-shadow:inherit; padding-top: inherit;">
  SELECT
    column_expression [, ...]
    FROM table_name
    [ WHERE where_expression [AND ...] ]
    [ GROUP BY column_name [, ...] ]
    [ ORDER BY column_name [ASC | DESC] [, ...] ]

  column_expression :=
    column_name |
    aggregation_function([DISTINCT] column_name)

  aggregation_function :=
    COUNT | SUM | AVG | MIN | MAX | STDDEV

  where_expression :=
    column_name comparison_operator value |
    column_name IS [NOT] NULL |
    column_name IN (constant [, ...])
    column_name [NOT] LIKE | ILIKE string_pattern
</pre>

__Notes__:

- The `*` argument and the `DISTINCT` modifier can only be provided to the `COUNT` function.
- The operator `OR` is currently not supported.
- The operator `NOT` can only be used in the cases mentioned above (`IS NOT NULL`, `NOT LIKE`, and `NOT ILIKE`).

## Understanding query results

`SELECT` queries return anonymized results. The results have a small amount of noise added to them. This is crucial in protecting the privacy of individuals, while sufficiently unobtrusive to provide accurate results during normal use.

### Low-count filtering

The results only contain data that appeared for a sufficiently large number of distinct users. For example, consider the query `SELECT first_name FROM users`.

Let's say that the names in the `users` table are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 2
John   | 150
Mary   | 1
Tom    | 2

Since the number of distinct for users named Bob, Mary, and Tom is too small, they won't appear in the final result. In contrast, there is a sufficient number of Alices and Johns, so the result will contain the corresponding rows.

In place of the discarded rows, the `*` rows will be included in the result. All columns of these rows will have the value of `*`. So in this example, the returned rows would have the following distribution:

Name   | Number of returned rows
------ | ------------------------
Alice  | 100
John   | 150
*      | 5

The number of `*` rows indicates the amount of properties that can't be included in the result. Note that this doesn't represent the number of _distinct_ omitted values. In this example, three distinct names are not reported (Bob, Mary, and Tom), but since there are two Bobs, one Mary, and two Toms, the result contains `2 + 1 + 2 = 5` `*` rows.

It's worth noting that absence of `*` rows doesn't mean that no rows were omitted. The `*` rows have to pass the same anonymization procedure. Thus, if the total count of `*` rows is too low, they will be omitted from the result.

### Adding noise

As explained in [Understanding query results](#understanding-query-results), some amount of noise is introduced in the result. Consider the example from the [previous section](#low-count-filtering), where there are 100 Alices, 150 Johns, and 5 other names. The anonymized result might contain a slightly different distribution, for example 94 Alice rows, 152 John rows, and 7 `*` rows.

The results of aggregate functions, such as `SUM` and `COUNT`, are also anonymized. The returned values will slightly differ from the real values.
