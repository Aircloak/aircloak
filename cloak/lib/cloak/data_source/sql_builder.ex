defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Sql.Query
  alias Cloak.Sql.Expression
  alias Cloak.DataSource.SqlBuilder.Support
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query), do: build(query, Cloak.DataSource.sql_dialect_module(query.data_source))

  @spec build(Query.t, atom) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query, sql_dialect_module), do:
    query |> build_fragments(sql_dialect_module) |> to_string()


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp column_name(%Expression{table: :unknown, name: name}, sql_dialect_module), do:
    quote_name(name, sql_dialect_module)
  defp column_name(column, sql_dialect_module), do:
    "#{quote_name(column.table.name, sql_dialect_module)}.#{quote_name(column.name, sql_dialect_module)}"

  defp build_fragments(query, sql_dialect_module) do
    [
      "SELECT ", columns_sql(query.db_columns, sql_dialect_module),
      " FROM ", from_clause(query.from, query, sql_dialect_module),
      where_fragments(query.where, sql_dialect_module),
      group_by_fragments(query, sql_dialect_module),
      having_fragments(query, sql_dialect_module),
      order_by_fragments(query, sql_dialect_module),
      range_fragments(query, sql_dialect_module)
    ]
  end

  defp columns_sql(columns, sql_dialect_module) do
    columns
    |> Enum.map(&alias_constant/1)
    |> Enum.map(&column_sql(&1, sql_dialect_module))
    |> Enum.intersperse(?,)
  end

  # Some data source drivers (e.g. mssql) require that we provide aliases for constants. Therefore, we're generating
  # an alias for non-aliased constants to satisfy that requirement. Note that we're doing it for all data sources.
  # This is not a problem, since we don't depend on column names anyway, and by doing it always, we avoid needless
  # polymorphism.
  defp alias_constant(%Expression{constant?: true, alias: empty_alias} = constant) when empty_alias in [nil, ""], do:
    %Expression{constant | alias: "alias_#{System.unique_integer([:positive])}"}
  defp alias_constant(other), do:
    other

  defp column_sql(:*, _sql_dialect_module), do: "*"
  defp column_sql({:distinct, column}, sql_dialect_module), do: ["DISTINCT ", column_sql(column, sql_dialect_module)]
  defp column_sql(%Expression{alias: alias} = column, sql_dialect_module) when alias != nil and alias != "", do:
    [column_sql(%Expression{column | alias: nil}, sql_dialect_module), " AS ", quote_name(alias, sql_dialect_module)]
  defp column_sql(%Expression{function?: true, function: fun_name, type: type, function_args: args}, sql_dialect_module)
    when fun_name in ["+", "-"] and type in [:time, :date, :datetime],
  do:
    sql_dialect_module.time_arithmetic_expression(fun_name, Enum.map(args, &to_fragment(&1, sql_dialect_module)))
  defp column_sql(%Expression{
    function?: true, function: "-", type: :interval, function_args: args
  }, sql_dialect_module), do:
    sql_dialect_module.date_subtraction_expression(Enum.map(args, &to_fragment(&1, sql_dialect_module)))
  defp column_sql(%Expression{function?: true, function: fun_name, function_args: args}, sql_dialect_module)
    when fun_name != nil,
  do:
    Support.function_sql(fun_name, Enum.map(args, &to_fragment(&1, sql_dialect_module)), sql_dialect_module)
  defp column_sql(%Expression{constant?: true, type: :like_pattern, value: value}, _sql_dialect_module), do:
    like_pattern_to_fragment(value)
  defp column_sql(%Expression{constant?: true, value: value}, sql_dialect_module), do:
    constant_to_fragment(value, sql_dialect_module)
  defp column_sql(%Expression{function?: false, constant?: false} = column, sql_dialect_module) do
    cond do
      DataDecoder.encoded_type(column) == :text ->
        # Force casting to text ensures we consistently fetch a string column as unicode, regardless of how it's
        # represented in the database (VARCHAR or NVARCHAR).
        Support.function_sql({:cast, :text}, [column_name(column, sql_dialect_module)], sql_dialect_module)
      column.type == :unknown ->
        sql_dialect_module.cast_unknown_sql(column_name(column, sql_dialect_module))
      true ->
        column_name(column, sql_dialect_module)
    end
  end

  defp from_clause({:join, join}, query, sql_dialect_module) do
    ["(", from_clause(join.lhs, query, sql_dialect_module), " ", join_sql(join.type), " ",
      from_clause(join.rhs, query, sql_dialect_module), on_clause(join.conditions, sql_dialect_module), ")"]
  end
  defp from_clause({:subquery, subquery}, _query, sql_dialect_module) do
    ["(", build_fragments(subquery.ast, sql_dialect_module), ") AS ", quote_name(subquery.alias, sql_dialect_module)]
  end
  defp from_clause(table_name, query, sql_dialect_module) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(sql_dialect_module)
  end

  defp on_clause(nil, _sql_dialect_module), do: []
  defp on_clause(condition, sql_dialect_module),
    do: [" ON ", conditions_to_fragments(condition, sql_dialect_module)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, sql_dialect_module), do:
    quote_name(table_name, sql_dialect_module)
  defp table_to_from(table, sql_dialect_module), do:
    "#{table.db_name} AS #{quote_name(table.name, sql_dialect_module)}"

  defp where_fragments(nil, _sql_dialect_module), do: []
  defp where_fragments(where_clause, sql_dialect_module),
    do: [" WHERE ", conditions_to_fragments(where_clause, sql_dialect_module)]

  defp conditions_to_fragments({:and, lhs, rhs}, sql_dialect_module), do:
    ["(", conditions_to_fragments(lhs, sql_dialect_module), ") AND (",
      conditions_to_fragments(rhs, sql_dialect_module), ")"]
  defp conditions_to_fragments({:or, lhs, rhs}, sql_dialect_module), do:
    ["(", conditions_to_fragments(lhs, sql_dialect_module), ") OR (",
      conditions_to_fragments(rhs, sql_dialect_module), ")"]
  defp conditions_to_fragments({:comparison, what, comparator, value}, sql_dialect_module), do:
    [to_fragment(what, sql_dialect_module), " #{comparator} ", to_fragment(value, sql_dialect_module)]
  defp conditions_to_fragments({:in, what, values}, sql_dialect_module), do:
    [to_fragment(what, sql_dialect_module), " IN (",
      Enum.map(values, &to_fragment(&1, sql_dialect_module)) |> join(", "), ")"]
  defp conditions_to_fragments({:like, what, match}, sql_dialect_module), do:
    sql_dialect_module.like_sql(to_fragment(what, sql_dialect_module), to_fragment(match, sql_dialect_module))
  defp conditions_to_fragments({:ilike, what, match}, sql_dialect_module), do:
    sql_dialect_module.ilike_sql(to_fragment(what, sql_dialect_module), to_fragment(match, sql_dialect_module))
  defp conditions_to_fragments({:is, what, match}, sql_dialect_module), do:
    [to_fragment(what, sql_dialect_module), " IS ", to_fragment(match, sql_dialect_module)]
  defp conditions_to_fragments({:not, condition}, sql_dialect_module), do:
    ["NOT ", conditions_to_fragments(condition, sql_dialect_module)]

  defp to_fragment(string, _sql_dialect_module) when is_binary(string), do: string
  defp to_fragment(atom, _sql_dialect_module) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(distinct = {:distinct, _}, sql_dialect_module), do: column_sql(distinct, sql_dialect_module)
  defp to_fragment(%Expression{alias: alias} = column, sql_dialect_module) when alias != nil and alias != "",
    do: column_sql(%Expression{column | alias: nil}, sql_dialect_module)
  defp to_fragment(%Expression{} = column, sql_dialect_module), do: column_sql(column, sql_dialect_module)

  defp constant_to_fragment(%NaiveDateTime{} = value, _sql_dialect_module), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Time{} = value, _sql_dialect_module), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Date{} = value, _sql_dialect_module), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Timex.Duration{} = value, sql_dialect_module), do:
    sql_dialect_module.interval_literal(value)
  defp constant_to_fragment(value, _sql_dialect_module) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value, _sql_dialect_module) when is_boolean(value), do: to_string(value)
  defp constant_to_fragment(value, sql_dialect_module) when is_binary(value), do:
    value
    |> escape_string()
    |> sql_dialect_module.unicode_literal()

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp like_pattern_to_fragment({pattern, escape = "\\"}), do: [?', pattern, ?', "ESCAPE", ?', escape, ?']

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}, sql_dialect_module),
    do: [" GROUP BY ", group_by |> Enum.map(&column_sql(&1, sql_dialect_module)) |> Enum.intersperse(", ")]
  defp group_by_fragments(_query, _sql_dialect_module), do: []

  defp having_fragments(%Query{subquery?: true, having: having_clause}, sql_dialect_module) when having_clause != nil,
    do: [" HAVING ", conditions_to_fragments(having_clause, sql_dialect_module)]
  defp having_fragments(_query, _sql_dialect_module), do: []

  defp order_by_fragments(%Query{subquery?: true, order_by: [_|_] = order_by}, sql_dialect_module) do
    order_by = for {expression, dir} <- order_by do
      dir = if dir == :desc do " DESC" else " ASC" end
      name = column_sql(expression, sql_dialect_module)
      [name, dir]
    end
    [" ORDER BY ", Enum.intersperse(order_by, ", ")]
  end
  defp order_by_fragments(_query, _sql_dialect_module), do: []

  defp quote_name(name, _sql_dialect_module), do: "\"#{name}\""

  defp range_fragments(%Query{subquery?: true, order_by: []}, _sql_dialect_module), do:
    []
  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, sql_dialect_module), do:
    sql_dialect_module.limit_sql(limit, offset)
  defp range_fragments(_query, _sql_dialect_module), do:
    []
end
