# Query Language

To write queries you use SQL. Aircloak supports a subset of standard SQL, implemented in a way that prevents leakage of
sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you
can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS
FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain anonymised data from tables. See [Understanding query
results](sql/query-results.md) for an explanation of the effects of anonymisation on the results.

The syntax conforms to the standard SQL syntax (with some exceptions), but only a subset of features is supported. The
general shape of the query looks like:

```SQL
SELECT [DISTINCT | ALL]
  field_expression [, ...]
  FROM from_expression [, ...]
  [ WHERE where_expression [AND ...] ]
  [ GROUP BY column_expression | position [, ...] ]
  [ HAVING having_expression [AND ...] ]
  [ ORDER BY column_name  | position [ASC | DESC] [NULLS FIRST | LAST] [, ...] [ LIMIT amount ] [ OFFSET amount ] ]
  [ SAMPLE_USERS <0..100>% ]

field_expression :=
  * | table_name.* | column_expression [AS alias]

column_expression :=
  [table_name.]column_name |
  aggregation_function([DISTINCT | ALL] column_name) |
  function(column_expression) |
  column_expression binary_operator column_expression |
  column_expression::data_type

binary_operator :=
  + | - | * | / | ^

data_type :=
  integer | real | text | boolean | datetime | date | time

from_expression :=
  table | join

table :=
  table_name [[AS] alias] | (select_expression) [AS] alias

join :=
  table CROSS JOIN table |
  table { [INNER] | { LEFT | RIGHT } [OUTER] } JOIN table ON where_expression

aggregation_function :=
  COUNT | SUM | AVG | MIN | MAX | STDDEV | MEDIAN |
  COUNT_NOISE | SUM_NOISE | AVG_NOISE | STDDEV_NOISE

where_expression :=
  column_expression equality_operator (value | column_expression) |
  column_expression inequality_operator (numerical_value | datetime_value) |
  column_expression BETWEEN value AND value |
  column_expression IS [NOT] NULL |
  column_expression [NOT] IN (constant [, ...]) |
  column_expression [NOT] LIKE | ILIKE string_pattern [ESCAPE escape_string] |
  column_expression [NOT] boolean_column_expression
  column_expression

having_expression :=
    column_expression comparison_operator (value | column_expression) |
    column_expression

comparison_operator :=
    equality_operator | inequality_operator

equality_operator :=
    = | <>

inequality_operator :=
    > | >= | < | <=
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
- If an integer is specified in the `GROUP BY` or `ORDER BY` clause, it represents a 1-based position in the select list. The
  corresponding expression from the select list is used as the grouping or ordering expression.
- Values of type `datetime with timezone` are not supported. The timezone information will be dropped and the value will
  be exposed as a simple `datetime` in the UTC format.
- The `SAMPLE_USERS` clause is an Aircloak specific feature that can be used to reduce the amount of users queried.
  This speeds up query execution and is useful for estimating the results when querying large datasets.
  The given parameter specifies the probability that an user will be included in the input. The sampling is not
  100% exact (especially for small inputs), but it is deterministic. The included users are an approximation of the
  requested percent from the total number of users, but the same users will be included each time a query is executed.
  Aggregates present in the query are not automatically adjusted. For example the `COUNT` returned when
  `SAMPLE_USERS 10%` is used will be approximately 10% of the count returned when the `SAMPLE_USERS`-clause
  is omitted. Fractions (like `SAMPLE_USERS 0.1%`) can be used when samples smaller than 1% are desired.
- The order of rows in subqueries is not preserved in the outer query. Add an `ORDER BY` clause in the outer query
  if you want a specific order.
- When `NULL` handling is not specified in an `ORDER BY` in a subquery (either `NULLS FIRST` or `NULLS LAST`) the
  default handling for the underlying datasource will be used. For postgres that means that `NULL` values will be
  treated as larger than all other values. For SAP HANA, MySQL, SQL Server, and MongoDB they will be treated as smaller
  than all other values. The top-level query always defaults to treating `NULL` values as larger than other values.
- Using a `column_expression` in place of a `where_expression` or a `having_expression` will implicitly compare the
  value of that `column_expression` to `TRUE`. In other words: `WHERE active` is equivalent to `WHERE active = TRUE`.


## Query and subquery types

Aircloak Insights supports both queries over sensitive data and queries over non-sensitive data. In this context sensitive
data is data pertaining to individual entities, as opposed to an anonymised aggregate across multiple such entities.

Queries that process sensitive data are subject to various [restrictions](sql/restrictions.md), and are called restricted
queries. Restricted queries can be arbitrarily nested. The top-most restricted query anonymises the data by producing
anonymised aggregates and filtering values that would allow an individual entity to be identified. Such a top-most query
is called an anonymising query.

An anonymising query could itself be a subquery to another query that further processes the anonymised query results or
combines it with that of a user-less table. Such a query is called a standard query. Standard queries have the usual SQL
validations applied to them, but do not underly the anonymisation related restrictions of the restricted queries.
Standard queries can only refer to user-less tables or to other standard or anonymising subqueries.

The following is an example:

```SQL
-- Standard query (only processes anonymised data)
SELECT
  min(age), max(age),
  count(age), sum(individuals) as num_users
FROM (
  -- Anonymising (and restricted) query
  SELECT age, count(*) as individuals FROM (
    -- Restricted query
    SELECT uid, t1.age
    FROM table1 t1 INNER JOIN table2 t2
      ON t1.uid = t2.uid
  ) t
  GROUP BY age
) b
```
