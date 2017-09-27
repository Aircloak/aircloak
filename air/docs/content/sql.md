# Query Language

To write queries you use SQL. Aircloak supports a subset of standard SQL, implemented in a way that prevents leakage of sensitive data.


## Exploring the database

You can discover database tables and their structure using the `SHOW` statement. To list the tables in the database, you can use the `SHOW TABLES` statement. If you want to see the columns of a particular table, you can invoke `SHOW COLUMNS FROM table_name`.


## Querying the database

The `SELECT` statement can be used to obtain anonymized data from tables. See [Understanding query results](sql/query-results.md) for an explanation of the effects of anonymization on the results.

The syntax conforms to the standard SQL syntax (with some exceptions), but only a subset of features is supported. The general shape of the query looks like:

```SQL
SELECT [DISTINCT]
  field_expression [, ...]
  FROM from_expression [, ...]
  [ SAMPLE_USERS <1..100>% ]
  [ WHERE where_expression [AND ...] ]
  [ GROUP BY column_expression | position [, ...] ]
  [ HAVING having_expression [AND ...] ]
  [ ORDER BY column_name [ASC | DESC] | position [, ...] [ LIMIT amount ] [ OFFSET amount ] ]

field_expression :=
  * | table_name.* | column_expression [AS alias]

column_expression :=
  [table_name.]column_name |
  aggregation_function([DISTINCT] column_name) |
  function(column_expression) |
  column_expression binary_operator column_expression |
  column_expression::data_type

binary_operator :=
  + | - | * | / | ^ | %

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
  column_expression [NOT] IN (constant [, ...])
  column_expression [NOT] LIKE | ILIKE string_pattern [ESCAPE escape_string]

having_expression :=
    column_expression comparison_operator (value | column_expression)

comparison_operator :=
    equality_operator | inequality_operator

equality_operator :=
    = | <>

inequality_operator :=
    > | >= | < | <=
```

## Notes

- The `*` argument can only be provided to the `COUNT` and `COUNT_NOISE` aggregators and it specifies counting rows
  instead of otherwise counting only non-`NULL` values. `NULL` values are ignored by all other aggregators.
- The operator `OR` is not supported.
- The operator `NOT` can only be used in the cases mentioned above (`IS NOT NULL`, `NOT LIKE`, and `NOT ILIKE`).
- You can restrict the range of returned rows by a query using the `LIMIT` and/or `OFFSET` clauses, but you need to
 provide the `ORDER BY` clause to ensure a stable order for the rows.
- Conditions in the `HAVING` clause must not refer to non-aggregated fields.
- Aliases can be used in the `WHERE`, `GROUP BY`, `ORDER BY` and `HAVING` clauses, as long as the alias doesn't conflict
 with a column name in one of the selected tables.
- If an integer is specified in the `GROUP BY` clause, it represents a 1-based position in the select list. The corresponding expression from the select list is used as the grouping expression.
- Values of type `datetime with timezone` are not supported. The timezone information will be dropped and the value will be exposed as a simple `datetime` in the UTC format.
- The `SAMPLE_USERS` clause is an Aircloak specific feature that can be used to reduce the amount of users queried. This speeds up query execution and is useful for estimating the results when querying large datasets. The sampling is not 100% exact, but it is deterministic. The included users are an approximation of the requested percent from the total number of users, but the same users will be included each time a query is executed. Aggregates present in the query are not automatically adjusted. For example the `COUNT` returned when `SAMPLE_USERS 10%` is used will be approximately 10% of the count returned when the `SAMPLE_USERS`-clause is omitted.
