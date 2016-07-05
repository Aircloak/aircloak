# Aircloak Query Language

To write queries, you can use Aircloak Query Language (AQL). AQL is a subset of standard SQL which supports only the features which will not cause leakage of sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain the anonymized data from tables. See [Understanding query results](understanding-query-results) for the explanation of the anonymized result.

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
- For privacy reasons, the operator `OR` is not allowed, while the operator `NOT` can be used only in the cases mentioned above (`IS NOT NULL`, `NOT LIKE`, and `NOT ILIKE`).

## Understanding query results

The result of a `SELECT` statement is anonymized. This means that some amount of noise is introduced to prevent a malicious attacker from obtaining sensitive data. Although the result is not 100% accurate, the introduced noise should be sufficiently small, especially for large sets. Therefore, analysts can draw reliable conclusions from the returned data, while attackers are prevented from learning something about particular individuals.

### Low-count filtering

The returned result will contain only rows which appear in a sufficiently large number of distinct users. For example, consider the query `SELECT first_name FROM users`.

Let's say that the names in the `users` table are distributed as follows:

Name   | Number of distinct users
------ | ------------------------
Alice  | 100
Bob    | 1
John   | 150
Mary   | 2

Since the number of distinct users named Bob and Mary is too small, they won't appear in the final result. In contrast, there is a sufficient number of Alices and Johns, so the result will contain the corresponding rows.

In place of the discarded rows, the `*` rows will be included in the result. All columns of these rows will have the value of `*`. However, these rows will be included only if their count is sufficient enough.

In the example above, we have three filtered rows (one Bob and two Marys). Since the count is still too small, `*` rows won't be included in the result.

However, let's say there are 100 users with infrequent names. In that case the total number of discarded rows is sufficiently large, so `*` rows will be included. The final result will contain rows for Alice, John, and a number of `*` rows. These `*` rows can be useful to analysts, because they represent the amount of values which can't be reported without leaking sensitive data. If there are many such rows, an analyst could then consider structuring the query in a different way.

### Adding noise

As explained in [Understanding query results](#understanding-query-results), some amount of noise is introduced in the result. Consider the example from the [previous section](#low-count-filtering), where there are 100 Alices and 150 Johns. The anonymized result might contain slightly different distribution, for example 94 rows named Alice and 152 rows named John. The same rule holds for the `*` rows. If included in the result, the number of `*` rows will be slightly skewed.

The results of aggregate functions, such as `SUM` and `COUNT`, are also anonymized. The returned values will slightly differ from the real values.
