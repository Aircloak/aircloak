defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.SqlBuilder.{Support, SQLServer, SAPIQ, MySQL}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t()) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query), do: build(query, query.data_source.driver.sql_dialect_module(), query.data_source.driver)

  @spec build(Query.t(), module, module) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query, sql_dialect_module, driver),
    do: query |> build_fragments(sql_dialect_module, driver) |> to_string()

  @doc "Makes sure the specified partial or full table name is quoted."
  @spec quote_table_name(String.t(), integer) :: String.t()
  def quote_table_name(table_name, quote_char \\ ?")

  def quote_table_name(<<quote_char::utf8, _::binary>> = table_name, quote_char), do: table_name

  def quote_table_name(table_name, quote_char),
    do:
      table_name
      |> String.split(".")
      |> Enum.map(&quote_name(&1, quote_char))
      |> Enum.join(".")

  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp column_name(%Expression{table: :unknown, name: name}, quote_char), do: quote_name(name, quote_char)

  defp column_name(column, quote_char),
    do: "#{quote_table_name(column.table.name, quote_char)}.#{quote_name(column.name, quote_char)}"

  defp build_fragments(query, sql_dialect_module, driver) do
    common_clauses = [
      columns_sql(query.db_columns, sql_dialect_module, driver),
      " FROM ",
      from_clause(query.from, query, sql_dialect_module, driver),
      where_fragments(query.where, sql_dialect_module, driver),
      group_by_fragments(query, sql_dialect_module, driver),
      having_fragments(query, sql_dialect_module, driver),
      order_by_fragments(query, sql_dialect_module, driver)
    ]

    range_clause = range_fragments(query, sql_dialect_module)

    if sql_dialect_module.range_at_statement_start?() do
      ["SELECT ", range_clause, " ", common_clauses]
    else
      ["SELECT ", common_clauses, range_clause]
    end
  end

  defp columns_sql(columns, sql_dialect_module, driver) do
    columns
    |> Enum.map(&column_sql(&1, sql_dialect_module, driver))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _sql_dialect_module, _driver), do: "*"

  defp column_sql({:distinct, column}, sql_dialect_module, driver),
    do: ["DISTINCT ", column_sql(column, sql_dialect_module, driver)]

  defp column_sql(%Expression{alias: alias} = column, sql_dialect_module, driver)
       when alias != nil and alias != "",
       do: [
         column_sql(%Expression{column | alias: nil}, sql_dialect_module, driver),
         " AS ",
         quote_name(alias, sql_dialect_module.quote_char())
       ]

  defp column_sql(
         %Expression{function?: true, function: fun_name, type: type, function_args: args},
         sql_dialect_module,
         driver
       )
       when fun_name in ["+", "-"] and type in [:time, :date, :datetime],
       do:
         sql_dialect_module.time_arithmetic_expression(
           fun_name,
           Enum.map(args, &to_fragment(&1, sql_dialect_module, driver))
         )

  defp column_sql(
         %Expression{
           function?: true,
           function: "-",
           type: :interval,
           function_args: args
         },
         sql_dialect_module,
         driver
       ),
       do: sql_dialect_module.date_subtraction_expression(Enum.map(args, &to_fragment(&1, sql_dialect_module, driver)))

  defp column_sql(
         %Expression{function: {:cast, to_type}, function_args: [arg]},
         sql_dialect_module,
         driver
       ),
       do: arg |> to_fragment(sql_dialect_module, driver) |> sql_dialect_module.cast_sql(arg.type, to_type)

  defp column_sql(expression = %Expression{function: "date_trunc", type: :date}, sql_dialect_module, driver) do
    column_sql(
      Expression.function({:cast, :date}, [%{expression | type: :datetime}], :date),
      sql_dialect_module,
      driver
    )
  end

  defp column_sql(
         %Expression{function?: true, function: fun_name, function_args: args},
         sql_dialect_module,
         driver
       ),
       do:
         Support.function_sql(
           fun_name,
           Enum.map(args, &to_fragment(&1, sql_dialect_module, driver)),
           sql_dialect_module
         )

  defp column_sql(
         %Expression{constant?: true, type: :like_pattern, value: value},
         _sql_dialect_module,
         _driver
       ),
       do: like_pattern_to_fragment(value)

  defp column_sql(%Expression{constant?: true, value: value}, sql_dialect_module, _driver),
    do: constant_to_fragment(value, sql_dialect_module)

  defp column_sql(%Expression{function?: false, constant?: false} = column, sql_dialect_module, driver),
    do: column |> column_name(sql_dialect_module.quote_char()) |> cast_type(column.type, sql_dialect_module, driver)

  defp cast_type(value, :unknown, sql_dialect_module, _driver), do: sql_dialect_module.cast_sql(value, :unknown, :text)

  defp cast_type(value, :text, sql_dialect_module, driver) do
    if driver.cast_to_text?(), do: sql_dialect_module.cast_sql(value, :text, :text), else: value
  end

  defp cast_type(value, _type, _sql_dialect_module, _driver), do: value

  defp from_clause({:join, join}, query, sql_dialect_module, driver) do
    [
      "(",
      from_clause(join.lhs, query, sql_dialect_module, driver),
      " ",
      join_sql(join.type),
      " ",
      from_clause(join.rhs, query, sql_dialect_module, driver),
      on_clause(join.conditions, sql_dialect_module, driver),
      ")"
    ]
  end

  defp from_clause({:subquery, subquery}, _query, sql_dialect_module, driver) do
    [
      "(",
      build_fragments(subquery.ast, sql_dialect_module, driver),
      ") AS ",
      quote_name(subquery.alias, sql_dialect_module.quote_char)
    ]
  end

  defp from_clause(table_name, query, sql_dialect_module, _driver) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(sql_dialect_module.quote_char())
  end

  defp on_clause(nil, _sql_dialect_module, _driver), do: []

  defp on_clause(condition, sql_dialect_module, driver),
    do: [" ON ", conditions_to_fragments(condition, sql_dialect_module, driver)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, quote_char), do: quote_table_name(table_name, quote_char)

  defp table_to_from(table, quote_char),
    do: "#{quote_table_name(table.db_name, quote_char)} AS #{quote_name(table.name, quote_char)}"

  defp where_fragments(nil, _sql_dialect_module, _driver), do: []

  defp where_fragments(where_clause, sql_dialect_module, driver),
    do: [" WHERE ", conditions_to_fragments(where_clause, sql_dialect_module, driver)]

  defp conditions_to_fragments({:and, lhs, rhs}, sql_dialect_module, driver),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module, driver),
      ") AND (",
      conditions_to_fragments(rhs, sql_dialect_module, driver),
      ")"
    ]

  defp conditions_to_fragments({:or, lhs, rhs}, sql_dialect_module, driver),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module, driver),
      ") OR (",
      conditions_to_fragments(rhs, sql_dialect_module, driver),
      ")"
    ]

  defp conditions_to_fragments(
         {:comparison, %Expression{type: :text} = what, comparator, value},
         sql_dialect_module,
         driver
       )
       when sql_dialect_module in [SQLServer, SAPIQ, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         "(",
         what |> dot_terminate() |> to_fragment(sql_dialect_module, driver),
         " #{comparator} ",
         value |> dot_terminate() |> to_fragment(sql_dialect_module, driver),
         ")"
       ]

  defp conditions_to_fragments({:comparison, what, comparator, value}, sql_dialect_module, driver),
    do: [
      to_fragment(what, sql_dialect_module, driver),
      " #{comparator} ",
      to_fragment(value, sql_dialect_module, driver)
    ]

  defp conditions_to_fragments({:in, %Expression{type: :text} = what, values}, sql_dialect_module, driver)
       when sql_dialect_module in [SQLServer, SAPIQ, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         what |> dot_terminate() |> to_fragment(sql_dialect_module, driver),
         " IN (",
         Enum.map(values, &(&1 |> dot_terminate() |> to_fragment(sql_dialect_module, driver))) |> join(", "),
         ")"
       ]

  defp conditions_to_fragments({:in, what, values}, sql_dialect_module, driver),
    do: [
      to_fragment(what, sql_dialect_module, driver),
      " IN (",
      Enum.map(values, &to_fragment(&1, sql_dialect_module, driver)) |> join(", "),
      ")"
    ]

  defp conditions_to_fragments({:like, what, match}, sql_dialect_module, driver),
    do:
      sql_dialect_module.like_sql(
        to_fragment(what, sql_dialect_module, driver),
        to_fragment(match, sql_dialect_module, driver)
      )

  defp conditions_to_fragments({:ilike, what, match}, sql_dialect_module, driver) do
    if sql_dialect_module.native_support_for_ilike?() do
      sql_dialect_module.ilike_sql(
        to_fragment(what, sql_dialect_module, driver),
        to_fragment(match, sql_dialect_module, driver)
      )
    else
      conditions_to_fragments(
        {:like, Expression.lowercase(what), Expression.lowercase(match)},
        sql_dialect_module,
        driver
      )
    end
  end

  defp conditions_to_fragments({:is, what, match}, sql_dialect_module, driver),
    do: [to_fragment(what, sql_dialect_module, driver), " IS ", to_fragment(match, sql_dialect_module, driver)]

  defp conditions_to_fragments({:not, condition}, sql_dialect_module, driver),
    do: ["NOT (", conditions_to_fragments(condition, sql_dialect_module, driver), ")"]

  defp to_fragment(string, _sql_dialect_module, _driver) when is_binary(string), do: string

  defp to_fragment(atom, _sql_dialect_module, _driver) when is_atom(atom), do: to_string(atom) |> String.upcase()

  defp to_fragment(distinct = {:distinct, _}, sql_dialect_module, driver),
    do: column_sql(distinct, sql_dialect_module, driver)

  defp to_fragment(%Expression{alias: alias} = column, sql_dialect_module, driver)
       when alias != nil and alias != "",
       do: column_sql(%Expression{column | alias: nil}, sql_dialect_module, driver)

  defp to_fragment(%Expression{} = column, sql_dialect_module, driver),
    do: column_sql(column, sql_dialect_module, driver)

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
         sql_dialect_module,
         driver
       ),
       do: [
         " GROUP BY ",
         group_by |> Enum.map(&column_sql(&1, sql_dialect_module, driver)) |> Enum.intersperse(", ")
       ]

  defp group_by_fragments(_query, _sql_dialect_module, _driver), do: []

  defp having_fragments(%Query{subquery?: true, having: having_clause}, sql_dialect_module, driver)
       when having_clause != nil,
       do: [" HAVING ", conditions_to_fragments(having_clause, sql_dialect_module, driver)]

  defp having_fragments(_query, _sql_dialect_module, _driver), do: []

  defp order_by_fragments(
         %Query{subquery?: true, order_by: [_ | _] = order_by},
         sql_dialect_module,
         driver
       ) do
    order_by =
      for {expression, dir, nulls} <- order_by do
        column = expression |> Expression.unalias() |> column_sql(sql_dialect_module, driver)
        sql_dialect_module.order_by(column, dir, nulls)
      end

    [" ORDER BY ", Enum.intersperse(order_by, ", ")]
  end

  defp order_by_fragments(_query, _sql_dialect_module, _driver), do: []

  defp quote_name(name, quote_char), do: <<quote_char::utf8, name::binary, quote_char::utf8>>

  defp range_fragments(%Query{subquery?: true, order_by: []}, _sql_dialect_module), do: []

  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset}, sql_dialect_module),
    do: sql_dialect_module.limit_sql(limit, offset)

  defp range_fragments(_query, _sql_dialect_module), do: []
end
