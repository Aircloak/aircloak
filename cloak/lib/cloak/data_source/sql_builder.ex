defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Sql.Query
  alias Cloak.Sql.Expression
  alias Cloak.DataSource.SqlBuilder.Support
  alias Cloak.Query.ExecutionError


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query), do: build(query, query.data_source.driver_dialect)

  @spec build(Query.t, atom) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query, sql_dialect), do:
    query |> build_fragments(sql_dialect) |> to_string()


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp column_name(%Expression{table: :unknown, name: name}, sql_dialect), do: quote_name(name, sql_dialect)
  defp column_name(column, sql_dialect), do:
    "#{quote_name(column.table.name, sql_dialect)}.#{quote_name(column.name, sql_dialect)}"

  defp build_fragments(query, sql_dialect) do
    [
      "SELECT ", distinct(query), columns_sql(query.db_columns, sql_dialect),
      " FROM ", from_clause(query.from, query, sql_dialect),
      where_fragments(query.where, sql_dialect),
      group_by_fragments(query, sql_dialect),
      having_fragments(query, sql_dialect),
      order_by_fragments(query, sql_dialect),
      range_fragments(query, sql_dialect)
    ]
  end

  defp distinct(%Query{distinct?: true, subquery?: true}), do: "DISTINCT "
  defp distinct(%Query{}), do: ""

  defp columns_sql(columns, sql_dialect) do
    columns
    |> Enum.map(&column_sql(&1, sql_dialect))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _sql_dialect), do: "*"
  defp column_sql({:distinct, column}, sql_dialect), do: ["DISTINCT ", column_sql(column, sql_dialect)]
  defp column_sql(%Expression{alias: alias} = column, sql_dialect) when alias != nil and alias != "",
    do: [column_sql(%Expression{column | alias: nil}, sql_dialect), " AS ", quote_name(alias, sql_dialect)]
  defp column_sql(%Expression{function?: true, function: fun_name, function_args: args}, sql_dialect)
    when fun_name != nil, do: Support.function_sql(fun_name, Enum.map(args, &to_fragment(&1, sql_dialect)), sql_dialect)
  defp column_sql(%Expression{constant?: true, type: :like_pattern, value: value}, _sql_dialect), do:
    like_pattern_to_fragment(value)
  defp column_sql(%Expression{constant?: true, value: value}, _sql_dialect), do: constant_to_fragment(value)
  # We can't directly select a field with an unknown type, so convert it to binary
  # This is needed in the case of using the ODBC driver with a GUID user id,
  # as the GUID type is not supported by the Erlang ODBC library
  defp column_sql(%Expression{type: :unknown, name: name} = column, :sqlserver) when name != nil, do:
    Support.function_sql({:cast, :varbinary}, [column_name(column, :sqlserver)], :sqlserver)
  defp column_sql(column, sql_dialect), do: column_name(column, sql_dialect)

  defp from_clause({:join, join}, query, sql_dialect) do
    ["(", from_clause(join.lhs, query, sql_dialect), " ", join_sql(join.type), " ",
      from_clause(join.rhs, query, sql_dialect), on_clause(join.conditions, sql_dialect), ")"]
  end
  defp from_clause({:subquery, subquery}, _query, sql_dialect) do
    ["(", build_fragments(subquery.ast, sql_dialect), ") AS ", quote_name(subquery.alias, sql_dialect)]
  end
  defp from_clause(table_name, query, sql_dialect) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(sql_dialect)
  end

  defp on_clause(nil, _sql_dialect), do: []
  defp on_clause(condition, sql_dialect),
    do: [" ON ", conditions_to_fragments(condition, sql_dialect)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, sql_dialect), do:
    quote_name(table_name, sql_dialect)
  defp table_to_from(table, sql_dialect), do:
    "#{table.db_name} AS #{quote_name(table.name, sql_dialect)}"

  defp where_fragments(nil, _sql_dialect), do: []
  defp where_fragments(where_clause, sql_dialect),
    do: [" WHERE ", conditions_to_fragments(where_clause, sql_dialect)]

  defp conditions_to_fragments({:and, lhs, rhs}, sql_dialect),
    do: ["(", conditions_to_fragments(lhs, sql_dialect), ") AND (", conditions_to_fragments(rhs, sql_dialect), ")"]
  defp conditions_to_fragments({:or, lhs, rhs}, sql_dialect),
    do: ["(", conditions_to_fragments(lhs, sql_dialect), ") OR (", conditions_to_fragments(rhs, sql_dialect), ")"]
  defp conditions_to_fragments({:comparison, what, comparator, value}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " #{comparator} ", to_fragment(value, sql_dialect)]
  defp conditions_to_fragments({:in, what, values}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " IN (",
      Enum.map(values, &to_fragment(&1, sql_dialect)) |> join(", "), ")"]
  defp conditions_to_fragments({:like, what, match}, :mysql = sql_dialect),
    do: [to_fragment(what, sql_dialect), " COLLATE latin1_general_cs LIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:like, what, match}, :sqlserver = sql_dialect),
    do: [to_fragment(what, sql_dialect), " COLLATE Latin1_General_CS_AS LIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:like, what, match}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " LIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:ilike, what, match}, :postgresql = sql_dialect),
    do: [to_fragment(what, sql_dialect), " ILIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:ilike, what, match}, :mysql = sql_dialect),
    do: [to_fragment(what, sql_dialect), " COLLATE latin1_general_ci LIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:ilike, what, match}, :sqlserver = sql_dialect),
    do: [to_fragment(what, sql_dialect), " COLLATE Latin1_General_CI_AS LIKE ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:is, what, match}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " IS ", to_fragment(match, sql_dialect)]
  defp conditions_to_fragments({:not, condition}, sql_dialect),
    do: ["NOT ", conditions_to_fragments(condition, sql_dialect)]

  defp to_fragment(string, _sql_dialect) when is_binary(string), do: string
  defp to_fragment(atom, _sql_dialect) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(distinct = {:distinct, _}, sql_dialect), do: column_sql(distinct, sql_dialect)
  defp to_fragment(%Expression{alias: alias} = column, sql_dialect) when alias != nil and alias != "",
    do: column_sql(%Expression{column | alias: nil}, sql_dialect)
  defp to_fragment(%Expression{} = column, sql_dialect), do: column_sql(column, sql_dialect)

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp constant_to_fragment(%NaiveDateTime{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Time{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Date{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(value) when is_binary(value), do: [?', escape_string(value), ?']
  defp constant_to_fragment(value) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value) when is_boolean(value), do: to_string(value)

  defp like_pattern_to_fragment({pattern, escape = "\\"}), do: [?', pattern, ?', "ESCAPE", ?', escape, ?']

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}, sql_dialect),
    do: [" GROUP BY ", group_by |> Enum.map(&column_sql(&1, sql_dialect)) |> Enum.intersperse(", ")]
  defp group_by_fragments(_query, _sql_dialect), do: []

  defp having_fragments(%Query{subquery?: true, having: having_clause}, sql_dialect) when having_clause != nil,
    do: [" HAVING ", conditions_to_fragments(having_clause, sql_dialect)]
  defp having_fragments(_query, _sql_dialect), do: []

  defp order_by_fragments(%Query{subquery?: true, order_by: [_|_] = order_by}, sql_dialect) do
    order_by = for {expression, dir} <- order_by do
      dir = if dir == :desc do " DESC" else " ASC" end
      name = column_sql(expression, sql_dialect)
      [name, dir]
    end
    [" ORDER BY ", Enum.intersperse(order_by, ", ")]
  end
  defp order_by_fragments(_query, _sql_dialect), do: []

  defp quote_name(name, :drill), do: "`#{name}`"
  defp quote_name(name, _sql_dialect), do: "\"#{name}\""

  defp range_fragments(%Query{subquery?: true, limit: nil, offset: 0}, _sql_dialect), do: []
  defp range_fragments(%Query{subquery?: true, limit: nil, offset: offset}, :postgresql), do:
    [" OFFSET ", to_string(offset)]
  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, :postgresql), do:
    [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]
  defp range_fragments(%Query{subquery?: true, limit: nil, offset: offset}, :mysql), do:
      [" LIMIT ", to_string(offset), ", 18446744073709551615"]
  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, :mysql), do:
    [" LIMIT ", to_string(offset), ", ", to_string(limit)]
  defp range_fragments(%Query{subquery?: true, limit: nil, offset: offset}, :sqlserver), do:
    [" OFFSET ", to_string(offset), " ROWS"]
  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, :sqlserver), do:
    [" OFFSET ", to_string(offset), " ROWS FETCH NEXT ", to_string(limit), " ROWS ONLY"]
  defp range_fragments(%Query{subquery?: true, limit: limit}, sql_dialect) when limit != nil, do:
    raise ExecutionError, message: "LIMIT clause is not supported on '#{sql_dialect}' data sources."
  defp range_fragments(%Query{subquery?: true, offset: offset}, sql_dialect) when offset > 0, do:
    raise ExecutionError, message: "OFFSET clause is not supported on '#{sql_dialect}' data sources."
  defp range_fragments(_query, _sql_dialect), do: []
end
