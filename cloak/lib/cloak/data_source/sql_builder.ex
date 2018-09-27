defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.SqlBuilder.{Support, SQLServer, MySQL}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t()) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query),
    do: to_string(build_fragments(query))

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

  defp sql_dialect_module(query), do: driver(query).sql_dialect_module

  defp driver(query), do: query.data_source.driver

  defp column_name(%Expression{table: :unknown, name: name}, quote_char), do: quote_name(name, quote_char)

  defp column_name(column, quote_char),
    do: "#{quote_table_name(column.table.name, quote_char)}.#{quote_name(column.name, quote_char)}"

  defp build_fragments(query) do
    common_clauses = [
      columns_sql(query.db_columns, query),
      " FROM ",
      from_clause(query.from, query),
      where_fragments(query.where, query),
      group_by_fragments(query),
      having_fragments(query),
      order_by_fragments(query)
    ]

    range_clause = range_fragments(query)

    if sql_dialect_module(query).range_at_statement_start?() do
      ["SELECT ", range_clause, " ", common_clauses]
    else
      ["SELECT ", common_clauses, range_clause]
    end
  end

  defp columns_sql(columns, query) do
    columns
    |> Enum.map(&column_sql(&1, query))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _query), do: "*"

  defp column_sql({:distinct, column}, query),
    do: ["DISTINCT ", column_sql(column, query)]

  defp column_sql(%Expression{alias: alias} = column, query)
       when alias != nil and alias != "",
       do: [
         column_sql(%Expression{column | alias: nil}, query),
         " AS ",
         quote_name(alias, sql_dialect_module(query).quote_char())
       ]

  defp column_sql(
         %Expression{function?: true, function: fun_name, type: type, function_args: args},
         query
       )
       when fun_name in ["+", "-"] and type in [:time, :date, :datetime],
       do:
         sql_dialect_module(query).time_arithmetic_expression(
           fun_name,
           Enum.map(args, &to_fragment(&1, query))
         )

  defp column_sql(
         %Expression{
           function?: true,
           function: "-",
           type: :interval,
           function_args: args
         },
         query
       ),
       do: sql_dialect_module(query).date_subtraction_expression(Enum.map(args, &to_fragment(&1, query)))

  defp column_sql(
         %Expression{function: {:cast, to_type}, function_args: [arg]},
         query
       ),
       do: arg |> to_fragment(query) |> sql_dialect_module(query).cast_sql(arg.type, to_type)

  defp column_sql(expression = %Expression{function: "date_trunc", type: :date}, query) do
    column_sql(
      Expression.function({:cast, :date}, [%{expression | type: :datetime}], :date),
      query
    )
  end

  defp column_sql(
         %Expression{function?: true, function: fun_name, function_args: args},
         query
       ),
       do:
         Support.function_sql(
           fun_name,
           Enum.map(args, &to_fragment(&1, query)),
           sql_dialect_module(query)
         )

  defp column_sql(
         %Expression{constant?: true, type: :like_pattern, value: value},
         _query
       ),
       do: like_pattern_to_fragment(value)

  defp column_sql(%Expression{constant?: true, value: value}, query),
    do: constant_to_fragment(value, query)

  defp column_sql(%Expression{function?: false, constant?: false} = column, query),
    do: column |> column_name(sql_dialect_module(query).quote_char()) |> cast_type(column.type, query)

  defp cast_type(value, :unknown, query), do: sql_dialect_module(query).cast_sql(value, :unknown, :text)

  defp cast_type(value, :text, query) do
    if driver(query).cast_to_text?(), do: sql_dialect_module(query).cast_sql(value, :text, :text), else: value
  end

  defp cast_type(value, _type, _query), do: value

  defp from_clause({:join, join}, query) do
    [
      " ",
      from_clause(join.lhs, query),
      " ",
      join_sql(join.type),
      " ",
      from_clause(join.rhs, query),
      on_clause(join.conditions, query),
      " "
    ]
  end

  defp from_clause({:subquery, subquery}, query) do
    [
      "(",
      build_fragments(subquery.ast),
      ") AS ",
      quote_name(subquery.alias, sql_dialect_module(query).quote_char)
    ]
  end

  defp from_clause(table_name, query) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(sql_dialect_module(query).quote_char())
  end

  defp on_clause(nil, _query), do: []

  defp on_clause(condition, query),
    do: [" ON ", conditions_to_fragments(condition, query)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, quote_char), do: quote_table_name(table_name, quote_char)

  defp table_to_from(table, quote_char),
    do: "#{quote_table_name(table.db_name, quote_char)} AS #{quote_name(table.name, quote_char)}"

  defp where_fragments(nil, _query), do: []

  defp where_fragments(where_clause, query),
    do: [" WHERE ", conditions_to_fragments(where_clause, query)]

  defp conditions_to_fragments(expression, query),
    do: conditions_to_fragments(expression, sql_dialect_module(query), query)

  defp conditions_to_fragments({:and, lhs, rhs}, sql_dialect_module, query),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module, query),
      ") AND (",
      conditions_to_fragments(rhs, sql_dialect_module, query),
      ")"
    ]

  defp conditions_to_fragments({:or, lhs, rhs}, sql_dialect_module, query),
    do: [
      "(",
      conditions_to_fragments(lhs, sql_dialect_module, query),
      ") OR (",
      conditions_to_fragments(rhs, sql_dialect_module, query),
      ")"
    ]

  defp conditions_to_fragments(
         {:comparison, %Expression{type: :text} = what, comparator, value},
         sql_dialect_module,
         query
       )
       when sql_dialect_module in [SQLServer, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         "(",
         what |> dot_terminate() |> to_fragment(query),
         " #{comparator} ",
         value |> dot_terminate() |> to_fragment(query),
         ")"
       ]

  defp conditions_to_fragments({:comparison, what, comparator, value}, _sql_dialect_module, query),
    do: [
      to_fragment(what, query),
      " #{comparator} ",
      to_fragment(value, query)
    ]

  defp conditions_to_fragments({:in, %Expression{type: :text} = what, values}, sql_dialect_module, query)
       when sql_dialect_module in [SQLServer, MySQL],
       # Some servers ignore trailing spaces during text comparisons.
       do: [
         what |> dot_terminate() |> to_fragment(query),
         " IN (",
         Enum.map(values, &(&1 |> dot_terminate() |> to_fragment(query))) |> join(", "),
         ")"
       ]

  defp conditions_to_fragments({:in, what, values}, _sql_dialect_module, query),
    do: [
      to_fragment(what, query),
      " IN (",
      Enum.map(values, &to_fragment(&1, query)) |> join(", "),
      ")"
    ]

  defp conditions_to_fragments({:like, what, match}, sql_dialect_module, query),
    do:
      sql_dialect_module.like_sql(
        to_fragment(what, query),
        to_fragment(match, query)
      )

  defp conditions_to_fragments({:ilike, what, match}, sql_dialect_module, query) do
    if sql_dialect_module.native_support_for_ilike?() do
      sql_dialect_module.ilike_sql(to_fragment(what, query), match.value)
    else
      conditions_to_fragments(
        {:like, Expression.lowercase(what), Expression.lowercase(match)},
        sql_dialect_module,
        query
      )
    end
  end

  defp conditions_to_fragments({:is, what, match}, _sql_dialect_module, query),
    do: [to_fragment(what, query), " IS ", to_fragment(match, query)]

  defp conditions_to_fragments({:not, condition}, sql_dialect_module, query),
    do: ["NOT (", conditions_to_fragments(condition, sql_dialect_module, query), ")"]

  defp to_fragment(string, _query) when is_binary(string), do: string

  defp to_fragment(atom, _query) when is_atom(atom), do: to_string(atom) |> String.upcase()

  defp to_fragment(distinct = {:distinct, _}, query),
    do: column_sql(distinct, query)

  defp to_fragment(%Expression{alias: alias} = column, query)
       when alias != nil and alias != "",
       do: column_sql(%Expression{column | alias: nil}, query)

  defp to_fragment(%Expression{} = column, query),
    do: column_sql(column, query)

  defp constant_to_fragment(%NaiveDateTime{} = value, _query), do: [?', to_string(value), ?']

  defp constant_to_fragment(%Time{} = value, _query), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Date{} = value, _query), do: [?', to_string(value), ?']

  defp constant_to_fragment(%Timex.Duration{} = value, query),
    do: sql_dialect_module(query).interval_literal(value)

  defp constant_to_fragment(value, _query) when is_number(value), do: to_string(value)

  defp constant_to_fragment(value, query) when is_boolean(value),
    do: sql_dialect_module(query).boolean_literal(value)

  defp constant_to_fragment(value, query) when is_binary(value),
    do: sql_dialect_module(query).unicode_literal(escape_string(value))

  defp constant_to_fragment(nil, _query), do: "NULL"

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp like_pattern_to_fragment({pattern, escape = "\\"}) do
    [?', pattern, ?', "ESCAPE", ?', escape, ?']
  end

  defp dot_terminate(%Expression{constant?: true, type: :text, value: value} = expression) when is_binary(value),
    do: %Expression{expression | value: value <> "."}

  defp dot_terminate(%Expression{type: :text} = expression),
    do: Expression.function("concat", [expression, Expression.constant(:text, ".")], :text)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_ | _] = group_by} = query),
    do: [
      " GROUP BY ",
      group_by |> Enum.map(&column_sql(&1, query)) |> Enum.intersperse(", ")
    ]

  defp group_by_fragments(_query), do: []

  defp having_fragments(%Query{subquery?: true, having: having_clause} = query)
       when having_clause != nil,
       do: [" HAVING ", conditions_to_fragments(having_clause, query)]

  defp having_fragments(_query), do: []

  defp order_by_fragments(%Query{subquery?: true, order_by: [_ | _] = order_by} = query) do
    order_by =
      for {expression, dir, nulls} <- order_by do
        column = expression |> Expression.unalias() |> column_sql(query)
        sql_dialect_module(query).order_by(column, dir, nulls)
      end

    [" ORDER BY ", Enum.intersperse(order_by, ", ")]
  end

  defp order_by_fragments(_query), do: []

  defp quote_name(name, quote_char), do: <<quote_char::utf8, name::binary, quote_char::utf8>>

  defp range_fragments(%Query{subquery?: true, order_by: []}), do: []

  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset} = query),
    do: sql_dialect_module(query).limit_sql(limit, offset)

  defp range_fragments(_query), do: []
end
