defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  use Combine
  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource.SqlBuilder.{Support, SQLServer, MySQL, Oracle, SAPHana}
  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t()) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query) do
    [select | rest] = build_fragments(query)
    to_string([select, sql_dialect_module(query).select_hints() | rest])
  end

  @doc """
  Makes sure the specified partial or full table name is quoted.

  ```
  iex> SqlBuilder.quote_table_name("name")
  ~s/"name"/

  iex> SqlBuilder.quote_table_name(~s/"name"/)
  ~s/"name"/

  iex> SqlBuilder.quote_table_name("full.name")
  ~s/"full"."name"/

  iex> SqlBuilder.quote_table_name(~s/"quoted.name"/)
  ~s/"quoted.name"/

  iex> SqlBuilder.quote_table_name("long.full.name")
  ~s/"long"."full"."name"/
  ```
  """
  @spec quote_table_name(String.t(), integer) :: String.t()
  def quote_table_name(table_name, quote_char \\ ?") do
    table_name
    |> table_name_parts()
    |> Enum.map(&quote_name(&1, quote_char))
    |> Enum.join(".")
  end

  @doc """
  Returns unquoted parts of the table name.

  Examples:

  ```
  iex> SqlBuilder.table_name_parts("foobar")
  ["foobar"]

  iex> SqlBuilder.table_name_parts("foo.bar")
  ["foo", "bar"]

  iex> SqlBuilder.table_name_parts(~s/"foo"."bar"/)
  ["foo", "bar"]

  iex> SqlBuilder.table_name_parts(~s/"foo".bar/)
  ["foo", "bar"]
  ```
  """
  @spec table_name_parts(String.t()) :: [String.t()]
  def table_name_parts(table_name) do
    case Combine.parse(table_name, sep_by1(part(), string(".")) |> eof()) do
      [parts] -> parts
      {:error, error} -> raise ArgumentError, error
    end
  end

  @doc "Escapes the given string."
  @spec escape_string(String.t()) :: String.t()
  def escape_string(string), do: String.replace(string, "'", "''")

  @doc "Builds the necessary JOIN chain to associate a `user_id` column with the table."
  @spec build_table_chain_with_user_id(%{atom => Table.t()}, atom) :: {String.t(), String.t()}
  def build_table_chain_with_user_id(tables, table_name) do
    {user_id_table_name, join_chain_list} =
      parse_user_id_join_chain(tables, table_name, tables[table_name].user_id_join_chain, quote_name(table_name))

    user_id_table = tables[user_id_table_name]
    {"#{quote_name(user_id_table_name)}.#{quote_name(user_id_table.user_id)}", to_string(join_chain_list)}
  end

  # -------------------------------------------------------------------
  # Table name parsing
  # -------------------------------------------------------------------

  defp part(), do: either(qualified_part(), unqualified_part()) |> map(&to_string/1)

  defp qualified_part() do
    sequence([
      ignore(string(~s/"/)),
      many1(either(escaped_quote(), satisfy(char(), &(&1 != ~s/"/)))),
      ignore(string(~s/"/))
    ])
  end

  defp escaped_quote(), do: map(string(~s/""/), fn _ -> ~s/"/ end)

  defp unqualified_part(), do: many1(satisfy(char(), &(&1 not in ~w(. "))))

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

  # MySQL requires that at least one column is selected; SQL Server requires that the column has a name;
  defp columns_sql([], query) do
    dialect = sql_dialect_module(query)
    dialect.alias_sql("0", quote_name("__ac_dummy", dialect.quote_char()))
  end

  defp columns_sql(columns, query) do
    columns
    |> Enum.map(&column_sql(&1, query))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _query), do: "*"

  defp column_sql({:distinct, column}, query),
    do: ["DISTINCT ", column_sql(column, query)]

  defp column_sql(%Expression{alias: alias} = column, query) when alias != nil and alias != "" do
    sql_dialect_module(query).alias_sql(
      column_sql(%Expression{column | alias: nil}, query),
      quote_name(alias, sql_dialect_module(query).quote_char())
    )
  end

  defp column_sql(%Expression{function?: true, function: fun_name, type: type, function_args: args}, query)
       when fun_name in ["+", "-"] and type in [:time, :date, :datetime],
       do: sql_dialect_module(query).time_arithmetic_expression(fun_name, Enum.map(args, &to_fragment(&1, query)))

  defp column_sql(%Expression{function?: true, function: "/", type: :interval, function_args: args}, query),
    do: sql_dialect_module(query).interval_division(Enum.map(args, &to_fragment(&1, query)))

  defp column_sql(%Expression{function?: true, function: "-", type: :interval, function_args: args}, query),
    do: sql_dialect_module(query).date_subtraction_expression(Enum.map(args, &to_fragment(&1, query)))

  defp column_sql(
         %Expression{function: {:cast, to_type}, function_args: [arg = %{function?: false, constant?: false}]},
         query
       ) do
    sql = arg |> to_fragment(query) |> sql_dialect_module(query).cast_sql(arg.type, to_type)

    if Query.database_column?(arg, query) do
      restrict(to_type, sql, arg.bounds)
    else
      sql
    end
  end

  defp column_sql(%Expression{function: {:cast, to_type}, function_args: [arg]}, query),
    do: arg |> to_fragment(query) |> sql_dialect_module(query).cast_sql(arg.type, to_type)

  defp column_sql(expression = %Expression{function: "date_trunc", type: :date}, query),
    do: column_sql(cast(%{expression | type: :datetime}, :date), query)

  defp column_sql(%Expression{function?: true, function: "sum", function_args: [arg], type: :real}, query) do
    # Force `SUM` of reals to use double precision.
    Support.function_sql("sum", [arg |> cast(:real) |> to_fragment(query)], sql_dialect_module(query))
  end

  defp column_sql(%Expression{function?: true, function: fun_name, function_args: args}, query) do
    args =
      if Cloak.Sql.Function.math_function?(fun_name) do
        Enum.map(args, &force_max_precision/1)
      else
        args
      end

    Support.function_sql(fun_name, Enum.map(args, &to_fragment(&1, query)), sql_dialect_module(query))
  end

  defp column_sql(%Expression{constant?: true, type: :like_pattern, value: value}, _query),
    do: like_pattern_to_fragment(value)

  defp column_sql(%Expression{constant?: true, value: value}, query),
    do: constant_to_fragment(value, query)

  defp column_sql(%Expression{function?: false, constant?: false, bounds: bounds} = column, query) do
    sql = column |> column_name(sql_dialect_module(query).quote_char()) |> cast_type(column.type, query)

    if Query.database_column?(column, query) do
      restrict(column.type, sql, bounds)
    else
      sql
    end
  end

  defp restrict(type, sql, {min, max}) when type in [:integer, :real],
    do: ["CASE WHEN ", sql, " < #{min} THEN #{min} WHEN ", sql, " > #{max} THEN #{max} ELSE ", sql, " END"]

  defp restrict(_type, sql, _bounds), do: sql

  defp force_max_precision(expression = %Expression{constant?: true}), do: expression
  defp force_max_precision(expression = %Expression{type: type}), do: cast(expression, type)

  defp cast_type(value, :unknown, query), do: sql_dialect_module(query).cast_sql(value, :unknown, :text)

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
    sql_dialect_module(query).alias_sql(
      ["(", build_fragments(subquery.ast), add_join_timing_protection(subquery), ")"],
      quote_name(subquery.alias, sql_dialect_module(query).quote_char)
    )
  end

  defp from_clause(table_name, query) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(query)
  end

  defp on_clause(nil, _query), do: []

  defp on_clause(condition, query),
    do: [" ON ", conditions_to_fragments(condition, query)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, query),
    do: quote_table_name(table_name, sql_dialect_module(query).quote_char())

  defp table_to_from(table, query) do
    sql_dialect_module(query).alias_sql(
      quote_table_name(table.db_name, sql_dialect_module(query).quote_char()),
      quote_name(table.name, sql_dialect_module(query).quote_char())
    )
  end

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

  defp constant_to_fragment(value, query) when is_binary(value),
    do: sql_dialect_module(query).literal(escape_string(value))

  defp constant_to_fragment(value, query),
    do: sql_dialect_module(query).literal(value)

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

  defp quote_name(name, quote_char \\ ?"), do: <<quote_char::utf8, to_string(name)::binary, quote_char::utf8>>

  defp range_fragments(%Query{subquery?: true, order_by: [], limit: nil, offset: 0}), do: []

  defp range_fragments(%Query{subquery?: true, limit: limit, offset: offset} = query),
    do: sql_dialect_module(query).limit_sql(limit, offset)

  defp range_fragments(_query), do: []

  defp parse_user_id_join_chain(_tables, current_table_name, [], join_chain_list),
    do: {current_table_name, join_chain_list}

  defp parse_user_id_join_chain(tables, current_table_name, [link | chain], acc) do
    {current_key, link_table_name, link_key} = link

    link_fragment = [
      " INNER JOIN ",
      quote_name(link_table_name),
      " ON ",
      quote_name(current_table_name),
      ".",
      quote_name(current_key),
      " = ",
      quote_name(link_table_name),
      ".",
      quote_name(link_key)
    ]

    parse_user_id_join_chain(tables, link_table_name, chain, [acc | link_fragment])
  end

  defp cast(%Expression{function: {:cast, to}} = expression, to), do: expression
  defp cast({:distinct, expression}, to), do: {:distinct, cast(expression, to)}
  defp cast(expression, to), do: Expression.function({:cast, to}, [expression], to)

  defp add_join_timing_protection(%{ast: query, join_timing_protection?: true}) do
    from =
      case sql_dialect_module(query) do
        Oracle -> " FROM dual"
        SAPHana -> " FROM dummy"
        _ -> ""
      end

    [
      " UNION ALL SELECT ",
      query.db_columns
      |> Enum.map(&Table.invalid_value(&1.type))
      |> Enum.map(&constant_to_fragment(&1, query))
      |> join(", "),
      from
    ]
  end

  defp add_join_timing_protection(_subquery), do: []
end
