# Query Language

To write queries you use SQL. Aircloak supports a subset of standard SQL, implemented in a way that prevents leakage of
sensitive data.

## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you
can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.

## Querying the database

The `SELECT` statement can be used to obtain anonymized data from tables. See [Understanding query
results](/sql/query-results.md) for an explanation of the effects of anonymization on the results.

The syntax conforms to the standard SQL syntax (with some exceptions), but only a subset of features is supported. The
general shape of the query looks like:

[filename](sql/_syntax.md ":include")

## Describing the query plan

To inspect a query without running it, you can prefix the uppermost `SELECT` statement with `EXPLAIN`.
The `EXPLAIN` query will return an overview with information about the query and its subqueries.

The following example shows the result of describing an [anonymizing restricted query](#anonymizing-restricted-queries)
with an inner [non-anonymizing restricted subquery](#non-anonymizing-restricted-subqueries):

```sql
EXPLAIN SELECT age, count(*) as individuals
FROM (
  SELECT uid, t1.age
  FROM table1 t1
  INNER JOIN table2 t2 ON t1.uid = t2.uid
) t
GROUP BY age
```

```
query (anonymized, statistics, 2 noise layers)
  --> regular_stats (Aircloak generated, restricted)
    --> uid_grouping (Aircloak generated, restricted)
      --> t (restricted)
        --> t1 (personal table)
        --> t2 (personal table)
```

## Considerations

- The `*` argument can only be provided to the `COUNT` and `COUNT_NOISE` aggregators and it specifies counting rows
  instead of otherwise counting only non-`NULL` values. `NULL` values are ignored by all other aggregators.
- The operator `OR` is not supported.
- The operator `NOT` can only be used in the cases mentioned above (`IS NOT NULL`, `NOT IN`, `NOT LIKE`, `NOT ILIKE`,
  and `NOT boolean_column_expression`).
- You can restrict the range of returned rows by a query using the `LIMIT` and/or `OFFSET` clauses, but you need to
  provide the `ORDER BY` clause to ensure a stable order for the rows.
- Conditions in the `HAVING` clause must not refer to non-aggregated fields.
- Aliases can be used in the `WHERE`, `GROUP BY`, `ORDER BY` and `HAVING` clauses, as long as the alias doesn't conflict
  with a column name in one of the selected tables.
- If an integer is specified in the `GROUP BY` or `ORDER BY` clause, it represents a 1-based position in the select
  list. The corresponding expression from the select list is used as the grouping or ordering expression.
- Values of type `datetime with timezone` are not supported. The timezone information will be dropped and the value will
  be exposed as a simple `datetime` in the UTC format.
- The order of rows in subqueries is not preserved in the outer query. Add an `ORDER BY` clause in the outer query
  if you want a specific order.
- When `NULL` handling is not specified in an `ORDER BY` in a subquery (either `NULLS FIRST` or `NULLS LAST`) the
  default handling for the underlying datasource will be used. For PostgreSQL that means that `NULL` values will be
  treated as larger than all other values. For SQL Server they will be treated as smaller
  than all other values. The top-level query always defaults to treating `NULL` values as larger than other values.
- Using a `column_expression` in place of a `filter_expression` will implicitly compare the value of that
  `column_expression` to `TRUE`. In other words: `WHERE active` is equivalent to `WHERE active = TRUE`.

## Query and subquery types

Aircloak Insights supports both queries over `personal` data and queries over `non-personal` data.
In this context `personal` data is data about a single person (or entity, depending on what it is
you want to protect through anonymization), and `non-personal` data facts from a fact or lookup table
(not relating to any one individual) or the result of an anonymizing subquery over personal data.

Queries that process `personal` data are subject to various [restrictions](/sql/restrictions.md),
and are called _restricted queries_. Restricted queries can be arbitrarily nested. The top-most
restricted query anonymizes the data. The anonymization produces a result that is about groups of
users rather than individuals, and filters out values that could identify an individual.
Such a top-most restricted query is called an _anonymizing query_.

An anonymizing query can itself be a subquery to another query. In such a case the data processed
by the other query is already anonymous and hence `non-personal`. Such a query is called a _standard query_.
A standard query can be used to further process an anonymized result set or, for example, to combine anonymized
data with data from a `non-personal` table such as a fact or lookup table. Standard queries have the usual
SQL validations applied to them (such as type checking), but the restrictions that are enforced for queries
processing personal data do not apply. Standard queries can only refer to `non-personal` tables or to other
standard or anonymizing subqueries.

### Distinguishing between query types

Being able to tell the query types apart helps you make sure you get the results you expect and want.
It is not uncommon that an analyst new to Aircloak Insights ends up writing a subquery producing aggregates
over multiple users, which becomes an anonymizing subquery, where they needed to write a subquery producing
per-user aggregates, which becomes a non-anonymizing restricted subquery. This mistake can cause most of the
data to get anonymized away, leading to results vastly different from what was expected. It can be hard to
debug and understand the cause of a situation like this if you do not know of the different query types and
how to distinguish between them.

You can use the [EXPLAIN statement](#describing-the-query-plan) to identify the type of a query and its subqueries.

#### Non-anonymizing restricted subqueries

You can tell that a subquery produces a per-user aggregate (is a _non-anonymizing restricted subquery_) if
_both of_ the following are true:

- it processes data from one or more personal tables or other non-anonymizing restricted subqueries
- it explicitly selects or groups by the column that was specified as the user id

Note that the top-most query can never be a non-anonymizing restricted subquery. The reason for this is that
all Aircloak queries have to produce an anonymized result. This is incompatible with the results of a non-anonymized
restricted subquery which is always personal data.

##### Some examples

This query is a non-anonymizing restricted subquery as it is a subquery and explicitly selects the user id column
from a table containing personal data:

```sql
... (
  SELECT uid, age
  FROM personal_table
) subquery
...
```

This query is a non-anonymizing restricted subquery as it is a subquery and explicitly creates a per-user aggregate
(through selecting and grouping by the user id column) from a table containing personal data:

```sql
... (
  SELECT uid, count(*) as numTransactions
  FROM transactions
  GROUP BY uid
) subquery
...
```

This query is a non-anonymizing restricted subquery as it is a subquery and explicitly creates per-user aggregates
(through selecting and grouping by the user id column) from a combination of tables containing personal data:

```sql
... (
  SELECT t.uid, count(*) as numTransactions, count(distinct a.id) as numAccounts
  FROM transactions t INNER JOIN accounts a ON t.account_id = a.id
  GROUP BY t.uid
) subquery
...
```

#### Anonymizing restricted queries

A query is an anonymizing query if it operates on personal data and _either of_ the following is true:

- it is the top-most query (Aircloak will never return a non-anonymized result)
- it aggregates over multiple users (i.e. does not explicitly group by the user id column)

##### Some examples

This query is anonymizing as it aggregates the personal data of multiple users into a single aggregate.
That the aggregate is over multiple users can be deduced from the query not grouping by the user id column.
Grouping by the user id column would produce per-user aggregates.

Even if the query hadn't been an aggregate over the personal data of multiple users, it would still
have been an anonymizing query. This is the case because it is the top-most query and processes personal data.
Had it not been anonymizing, it would have leaked personal information.

```sql
SELECT count(*)
FROM personal_table
```

The following subquery is anonymizing, despite grouping by a column that, in this particular example, likely is mostly
unique to a user. The intent might have been for the query to be a non-anonymizing restricted query producing a
per-user aggregate. It isn't marked as such by Aircloak Insights as it does not select and group by the column specified
as the user id column. This query is likely to produce no (or very little) results after anonymization as there will only
be few cases where multiple distinct individuals share the same phone number.

```sql
... (
  SELECT phone_number, count(*) as numTransactions
  FROM transactions
  GROUP BY phone_number
) per_phone_transactions
```

The following subquery is anonymizing as it produces an aggregate over the data of multiple users.
The resulting data can be further processed by a standard query.

```sql
... (
  SELECT city, count(*)
  FROM personal_table
  GROUP BY city
) city_distribution
```

Somewhat counterintuitively the following query is rejected despite being perfectly valid SQL.
The reason is that it is a personal query due to creating a per-user aggregate. We can tell that this
is the case by observing that the query selects (and groups by) the user id column.
At the same time the query is the top-most query and as such Aircloak is forced to anonymize its output.
Anonymizing the results of a query where each output row uniquely belongs to a single user (the uid column is
unique to a user) cannot be done without having to filter away all the data. Aircloak therefore rejects the query.

```sql
SELECT uid, count(*)
FROM personal_table
GROUP BY uid
```

#### Standard queries

A standard query is a query that executes over anonymized and/or non-personal data. In fact any query which is not
either an anonymizing query or a non-anonymizing restricted query is a standard query.

The following query combines all query types into a single composite query showing how they might relate:

```sql
-- Standard query (only processes anonymized (and hence non-personal) data)
SELECT
  min(age), max(age),
  count(age), sum(individuals) as num_users
FROM (
  -- An anonymizing (and restricted) query as it produces an aggregate
  -- that spans the data of multiple persons
  SELECT age, count(*) as individuals FROM (
    -- Restricted query as processes per-user data and explicitly
    -- selects the user id column (here named uid)
    SELECT uid, t1.age
    FROM table1 t1 INNER JOIN table2 t2
      ON t1.uid = t2.uid
  ) t
  GROUP BY age
) b
```
