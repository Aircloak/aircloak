defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.SqlBuilder.{Support, SQLServer, SAPIQ, MySQL}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t()) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query), do: build(query, Cloak.DataSource.sql_dialect_module(query.data_source))

  @spec build(Query.t(), atom) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query, sql_dialect_module), do: query |> build_fragments(sql_dialect_module) |> to_string()

  @doc "Makes sure the specified partial or full table name is quoted."
  @spec quote_table_name(String.t()) :: String.t()
  def quote_table_name("\"" <> _ = table_name), do: table_name

  def quote_table_name(table_name),
    do:
      table_name
      |> String.split(".")
      |> Enum.map(&quote_name/1)
      |> Enum.join(".")

  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp column_name(%Expression{table: :unknown, name: name}), do: quote_name(name)

  defp column_name(column), do: "#{quote_table_name(column.table.name)}.#{quote_name(column.name)}"

  defp build_fragments(query, sql_dialect_module) do
    common_clauses = [
      columns_sql(query.db_columns, sql_dialect_module),
      " FROM ",
      from_clause(query.from, query, sql_dialect_module),
      where_fragments(query.where, sql_dialect_module),
      group_by_fragments(query, sql_dialect_module),
      having_fragments(query, sql_dialect_module),
      order_by_fragments(query, sql_dialect_module)
    ]

    range_clause = range_fragments(query, sql_dialect_module)

    if sql_dialect_module.range_at_statement_start?() do
      ["SELECT ", range_clause, " ", common_clauses]
    else
      ["SELECT ", common_clauses, range_clause]
    end
  end

  defp columns_sql(columns, sql_dialect_module) do
    columns
    |> Enum.map(&column_sql(&1, sql_dialect_module))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _sql_dialect_module), do: "*"

  defp column_sql({:distinct, column}, sql_dialect_module), do: ["DISTINCT ", column_sql(column, sql_dialect_module)]

  defp column_sql(%Expression{alias: alias} = column, sql_dialect_module)
       when alias != nil and alias != "",
       do: [
         column_sql(%Expression{column | alias: nil}, sql_dialect_module),
         " AS ",
         quote_name(alias)
       ]

  defp column_sql(
         %Expression{function?: true, function: fun_name, type: type, function_args: args},
         sql_dialect_module
       )
       when fun_name in ["+", "-"] and type in [:time, :date, :datetime],
       do:
         sql_dialect_module.time_arithmetic_expression(
           fun_name,
           Enum.map(args, &to_fragment(&1, sql_dialect_module))
         )

  defp column_sql(
         %Expression{
           function?: true,
           function: "-",
           type: :interval,
           function_args: args
         },
         sql_dialect_module
       ),
       do: sql_dialect_module.date_subtraction_expression(Enum.map(args, &to_fragment(&1, sql_dialect_module)))

  defp column_sql(
         %Expression{function: {:cast, to_type}, function_args: [arg]},
         sql_dialect_module
       ),
       do: arg |> to_fragment(sql_dialect_module) |> sql_dialect_module.cast_sql(arg.type, to_type)

  defp column_sql(
         %Expression{function?: true, function: fun_name, function_args: args},
         sql_dialect_module
       ),
       do:
         Support.function_sql(
           fun_name,
           Enum.map(args, &to_fragment(&1, sql_dialect_module)),
           sql_dialect_module
         )

  defp column_sql(
         %Expression{constant?: true, type: :like_pattern, value: value},
         _sql_dialect_module
       ),
       do: like_pattern_to_fragment(value)

  defp column_sql(%Expression{constant?: true, value: value}, sql_dialect_module),
    do: constant_to_fragment(value, sql_dialect_module)

  defp column_sql(%Expression{function?: false, constant?: false} = column, sql_dialect_module),
    do: column |> column_name() |> cast_type(column.type, sql_dialect_module)

  defp cast_type(value, type, sql_dialect_module) when type in [:text, :unknown],
    # Force casting to text ensures we consistently fetch a string column as unicode, regardless of how it's
    # represented in the database (VARCHAR or NVARCHAR).
    do: sql_dialect_module.cast_sql(value, type, :text)

  defp cast_type(value, _type, _sql_dialect_module), do: value

  defp from_clause({:join, join}, query, sql_dialect_module) do
    [
      "(",
      from_clause(join.lhs, query, sql_dialect_module),
      " ",
      join_sql(join.type),
      " ",
      from_clause(join.rhs, query, sql_dialect_module),
      on_clause(join.conditions, sql_dialect_module),
      ")"
    ]
  end

  defp from_clause({:subquery, subquery}, _query, sql_dialect_module) do
    ["(", build_fragments(subquery.ast, sql_dialect_module), ") AS ", quote_name(subquery.alias)]
  end

  defp from_clause(table_name, query, _sql_dialect_module) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from()
  end

  defp on_clause(nil, _sql_dialect_module), do: []

  defp on_clause(condition, sql_dialect_module), do: [" ON ", conditions_to_fragments(condition, sql_dialect_module)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}), do: quote_table_name(table_name)
  defp table_to_from(table), do: "#{quote_table_name(table.db_name)} AS #{quote_name(table.name)}"

  defp where_fragments(nil, _sql_dialect_module), do: []

  defp where_fragments(where_clause, sql_dialect_module),
    do: [" WHERE ", conditions_to_fragments(where_clause, sql_dialect_module)]

  defp conditions_to_fragments({:and, lhs, rhs}, sql_dialect_module),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module),
      ") AND (",
      conditions_to_fragments(rhs, sql_dialect_module),
      ")"
    ]

  defp conditions_to_fragments({:or, lhs, rhs}, sql_dialect_module),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module),
      ") OR (",
      conditions_to_fragments(rhs, sql_dialect_module),
      ")"
    ]

  defp conditions_to_fragments({:comparison, %Expression{type: :text} = what, comparator, value}, sql_dialect_module)
       when sql_dialect_module in [SQLServer, SAPIQ, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         "(",
         what |> dot_terminate() |> to_fragment(sql_dialect_module),
         " #{comparator} ",
         value |> dot_terminate() |> to_fragment(sql_dialect_module),
         ")"
       ]

  defp conditions_to_fragments({:comparison, what, comparator, value}, sql_dialect_module),
    do: [
      to_fragment(what, sql_dialect_module),
      " #{comparator} ",
      to_fragment(value, sql_dialect_module)
    ]

  defp conditions_to_fragments({:in, %Expression{type: :text} = what, values}, sql_dialect_module)
       when sql_dialect_module in [SQLServer, SAPIQ, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         what |> dot_terminate() |> to_fragment(sql_dialect_module),
         " IN (",
         Enum.map(values, &(&1 |> dot_terminate() |> to_fragment(sql_dialect_module))) |> join(", "),
         ")"
       ]

  defp conditions_to_fragments({:in, what, values}, sql_dialect_module),
    do: [
      to_fragment(what, sql_dialect_module),
      " IN (",
      Enum.map(values, &to_fragment(&1, sql_dialect_module)) |> join(", "),
      ")"
    ]

  defp conditions_to_fragments({:like, what, match}, sql_dialect_module),
    do:
      sql_dialect_module.like_sql(
        to_fragment(what, sql_dialect_module),
        to_fragment(match, sql_dialect_module)
      )

  defp conditions_to_fragments({:ilike, what, match}, sql_dialect_module) do
    if sql_dialect_module.native_support_for_ilike?() do
      sql_dialect_module.ilike_sql(
        to_fragment(what, sql_dialect_module),
        to_fragment(match, sql_dialect_module)
      )
    else
      conditions_to_fragments(
        {:like, Expression.lowercase(what), Expression.lowercase(match)},
        sql_dialect_module
      )
    end
  end

  defp conditions_to_fragments({:is, what, match}, sql_dialect_module),
    do: [to_fragment(what, sql_dialect_module), " IS ", to_fragment(match, sql_dialect_module)]

  defp conditions_to_fragments({:not, condition}, sql_dialect_module),
    do: ["NOT ", conditions_to_fragments(condition, sql_dialect_module)]

  defp to_fragment(string, _sql_dialect_module) when is_binary(string), do: string

  defp to_fragment(atom, _sql_dialect_module) when is_atom(atom), do: to_string(atom) |> String.upcase()

  defp to_fragment(distinct = {:distinct, _}, sql_dialect_module), do: column_sql(distinct, sql_dialect_module)

  defp to_fragment(%Expression{alias: alias} = column, sql_dialect_module)
       when alias != nil and alias != "",
       do: column_sql(%Expression{column | alias: nil}, sql_dialect_module)

  defp to_fragment(%Expression{} = column, sql_dialect_module), do: column_sql(column, sql_dialect_module)

  defp constant_to_fragment(%NaiveDateTime{} = value, _sql_dialect_module), do: [?', to_string(value), ?']

  defp constant_to_fragment(%Time{} = value, _sql_dialect_module), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Date{} = value, _sql_dialect_module), do: [?', to_string(value), ?']

  defp constant_to_fragment(%Timex.Duration{} = value, sql_dialect_module),
    do: sql_dialect_module.interval_literal(value)

  defp constant_to_fragment(value, _sql_dialect_module) when is_number(value), do: to_string(value)

  defp constant_to_fragment(value, sql_dialect_module) when is_boolean(value),
    do: sql_dialect_module.boolean_literal(value)

  defp constant_to_fragment(value, sql_dialect_module) when is_binary(value),
    do:
      value
      |> escape_string()
      |> sql_dialect_module.unicode_literal()

  defp constant_to_fragment(nil, _sql_dialect_module), do: "NULL"

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp like_pattern_to_fragment({pattern, escape = "\\"}), do: [?', pattern, ?', "ESCAPE", ?', escape, ?']

  defp dot_terminate(%Expression{constant?: true, type: :text, value: value} = expression) when is_binary(value),
    do: %Expression{expression | value: value <> "."}

  defp dot_terminate(%Expression{type: :text} = expression),
    do: Expression.function("concat", [expression, Expression.constant(:text, ".")], :text)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(
         %Query{subquery?: true, group_by: [_ | _] = group_by},
         sql_dialect_module
       ),
       do: [
         " GROUP BY ",
         group_by |> Enum.map(&column_sql(&1, sql_dialect_module)) |> Enum.intersperse(", ")
       ]

  defp group_by_fragments(_query, _sql_dialect_module), do: []

  defp having_fragments(%Query{subquery?: true, having: having_clause}, sql_dialect_module)
       when having_clause != nil,
       do: [" HAVING ", conditions_to_fragments(having_clause, sql_dialect_module)]

  defp having_fragments(_query, _sql_dialect_module), do: []

  defp order_by_fragments(
         %Query{subquery?: true, order_by: [_ | _] = order_by},
         sql_dialect_module
       ) do
    order_by =
      for {expression, dir, nulls} <- order_by do
        column = expression |> Expression.unalias() |> column_sql(sql_dialect_module)
        sql_dialect_module.order_by(column, dir, nulls)
      end

    [" ORDER BY ", Enum.intersperse(order_by, ", ")]
  end

  defp order_by_fragments(_query, _sql_dialect_module), do: []

  defp quote_name(name), do: "\"#{name}\""

  defp range_fragments(%Query{subquery?: true, order_by: []}, _sql_dialect_module), do: []

  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, sql_dialect_module),
    do: sql_dialect_module.limit_sql(limit, offset)

  defp range_fragments(_query, _sql_dialect_module), do: []
end
