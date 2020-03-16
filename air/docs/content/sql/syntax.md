```SQL
SELECT [DISTINCT | ALL]
  field_expression [, ...]
  FROM from_expression [, ...]
  [ WHERE filter_expression [AND ...] ]
  [ GROUP BY column_expression | position [, ...] ]
  [ HAVING filter_expression [AND ...] ]
  [ ORDER BY column_name | position [ASC | DESC] [NULLS FIRST | LAST] [, ...]
  [ LIMIT amount [ OFFSET amount ] ]

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
  integer | real | text | boolean | datetime | date | time | interval

from_expression :=
  table | join

table :=
  table_name [[AS] alias] | (select_expression) [AS] alias

join :=
  table CROSS JOIN table |
  table { [INNER] | { LEFT | RIGHT } [OUTER] } JOIN table ON filter_expression

aggregation_function :=
  COUNT | SUM | AVG | MIN | MAX | STDDEV | VARIANCE | MEDIAN |
  COUNT_NOISE | SUM_NOISE | AVG_NOISE | STDDEV_NOISE | VARIANCE_NOISE

filter_expression :=
  column_expression equality_operator (value | column_expression) |
  column_expression inequality_operator (numerical_value | datetime_value) |
  column_expression BETWEEN value AND value |
  column_expression IS [NOT] NULL |
  column_expression [NOT] IN (constant [, ...]) |
  column_expression [NOT] LIKE | ILIKE string_pattern [ESCAPE escape_string] |
  column_expression [NOT] boolean_column_expression
  column_expression

comparison_operator :=
    equality_operator | inequality_operator

equality_operator :=
    = | <>

inequality_operator :=
    > | >= | < | <=
```
