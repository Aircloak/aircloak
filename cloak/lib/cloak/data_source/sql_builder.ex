defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  use Combine
  alias Cloak.Sql.{Query, Expression, Compiler, Function}
  alias Cloak.DataSource.SqlBuilder.{Support, ClouderaImpala}
  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec build(Query.t()) :: String.t()
  @doc "Constructs a parametrized SQL query that can be executed against a backend."
  def build(query),
    do:
      query
      |> Compiler.Helpers.apply_bottom_up(&mark_boolean_expressions/1)
      |> build_fragments()
      |> to_string()

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

  iex> SqlBuilder.quote_table_name("name", ?`)
  ~s/`name`/
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
    do: "#{quote_name(column.table.name, quote_char)}.#{quote_name(column.name, quote_char)}"

  defp build_fragments(%Query{command: :union, from: {:union, {:subquery, lhs}, {:subquery, rhs}}} = query) do
    [
      "(",
      build_fragments(lhs.ast),
      ") UNION ",
      if(query.distinct?, do: "", else: "ALL "),
      "(",
      build_fragments(rhs.ast),
      ")"
    ]
  end

  defp build_fragments(%Query{command: :select} = query) do
    common_clauses = [
      columns_sql(query),
      " FROM ",
      from_clause(query.from, query),
      where_fragments(query),
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

  # Some backends require that at least one column is selected; A few also require that the column has a name;
  defp columns_sql(%Query{db_columns: []} = query) do
    dialect = sql_dialect_module(query)
    dialect.alias_sql("0", quote_name("__ac_dummy", dialect.quote_char()))
  end

  defp columns_sql(query) do
    query.db_columns
    |> Enum.map(&column_sql(&1, sql_dialect_module(query)))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _dialect), do: "*"

  defp column_sql({:distinct, column}, dialect),
    do: ["DISTINCT ", column_sql(column, dialect)]

  defp column_sql(%Expression{alias: alias} = column, dialect) when alias not in [nil, ""] do
    dialect.alias_sql(
      column_sql(%Expression{column | alias: nil}, dialect),
      quote_name(alias, dialect.quote_char())
    )
  end

  defp column_sql(%Expression{kind: :function, name: fun_name, type: type, args: [arg1, arg2]}, dialect)
       when fun_name in ~w(+ - unsafe_add unsafe_sub) and type in [:time, :date, :datetime] do
    args = if arg1.type == :interval, do: [arg2, arg1], else: [arg1, arg2]
    dialect.time_arithmetic_expression(fun_name, Enum.map(args, &to_fragment(&1, dialect)))
  end

  defp column_sql(%Expression{kind: :function, name: fun_name, type: :interval, args: args}, dialect)
       when fun_name in ~w(- unsafe_sub) do
    [%Expression{type: type}, %Expression{type: type}] = args
    dialect.date_subtraction_expression(type, Enum.map(args, &to_fragment(&1, dialect)))
  end

  defp column_sql(%Expression{kind: :function, name: {:cast, to_type}, args: [arg]}, dialect),
    do: arg |> to_fragment(dialect) |> dialect.cast_sql(arg.type, to_type)

  defp column_sql(expression = %Expression{kind: :function, name: "date_trunc", type: :date}, dialect),
    do: column_sql(cast(%{expression | type: :datetime}, :date), dialect)

  defp column_sql(%Expression{kind: :function, name: "sum", args: [arg], type: :real}, dialect) do
    # Force `SUM` of reals to use double precision.
    Support.function_sql("sum", [arg |> cast(:real) |> to_fragment(dialect)], dialect)
  end

  defp column_sql(%Expression{kind: :function, name: fun_name, args: args}, dialect) do
    args =
      if Cloak.Sql.Function.math_function?(fun_name) do
        Enum.map(args, &force_max_precision/1)
      else
        args
      end

    Support.function_sql(fun_name, Enum.map(args, &to_fragment(&1, dialect)), dialect)
  end

  defp column_sql(%Expression{kind: :constant, value: value}, dialect),
    do: constant_to_fragment(value, dialect)

  defp column_sql(%Expression{kind: :column} = column, dialect),
    do: column |> column_name(dialect.quote_char()) |> cast_type(column.type, dialect)

  defp force_max_precision(expression = %Expression{kind: :constant}), do: expression
  defp force_max_precision(expression = %Expression{type: type}), do: cast(expression, type)

  defp cast_type(value, :unknown, dialect), do: dialect.cast_sql(value, :unknown, :text)

  defp cast_type(value, _type, _dialect), do: value

  defp from_clause({:join, join}, query) do
    [
      from_clause(join.lhs, query),
      " ",
      join_sql(join.type),
      " ",
      from_clause(join.rhs, query),
      on_clause(join.condition, query)
    ]
  end

  defp from_clause({:subquery, subquery}, query) do
    dialect = sql_dialect_module(query)

    dialect.alias_sql(
      ["(", build_subquery(subquery), ")"],
      quote_name(subquery.alias, dialect.quote_char())
    )
  end

  defp from_clause(table_name, query) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from(query)
  end

  defp on_clause(nil, _query), do: []

  defp on_clause(condition, query),
    do: [" ON ", column_sql(condition, sql_dialect_module(query))]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(table, query) do
    dialect = sql_dialect_module(query)
    db_name = quote_table_name(table.db_name, dialect.quote_char())
    name = quote_name(table.name, dialect.quote_char())

    if name == db_name,
      do: name,
      else: dialect.alias_sql(db_name, name)
  end

  defp where_fragments(%Query{where: nil}), do: []
  defp where_fragments(query), do: [" WHERE ", column_sql(query.where, sql_dialect_module(query))]

  defp to_fragment(string, _dialect) when is_binary(string), do: string

  defp to_fragment(atom, _dialect) when is_atom(atom), do: to_string(atom) |> String.upcase()

  defp to_fragment(distinct = {:distinct, _}, dialect),
    do: column_sql(distinct, dialect)

  defp to_fragment(%Expression{alias: alias} = column, dialect)
       when alias != nil and alias != "",
       do: column_sql(%Expression{column | alias: nil}, dialect)

  defp to_fragment(%Expression{} = column, dialect), do: column_sql(column, dialect)

  defp constant_to_fragment(:*, dialect), do: dialect.literal("*")

  defp constant_to_fragment(value, dialect) when is_binary(value), do: dialect.literal(escape_string(value))

  defp constant_to_fragment({pattern, _regex, _regex_ci}, dialect) do
    escaped_pattern = pattern |> escape_string() |> dialect.literal()

    if dialect.supports_overriding_pattern_escape?() do
      [escaped_pattern, " ESCAPE ", dialect.literal("\\")]
    else
      escaped_pattern
    end
  end

  defp constant_to_fragment(value, dialect), do: dialect.literal(value)

  defp dot_terminate(%Expression{kind: :constant, type: :text, value: value} = expression) when is_binary(value),
    do: %Expression{expression | value: value <> "."}

  defp dot_terminate(%Expression{type: :text} = expression),
    do: Expression.function("concat", [expression, Expression.constant(:text, ".")], :text)

  defp group_by_fragments(%Query{subquery?: true, grouping_sets: [_ | _]} = query) do
    dialect = sql_dialect_module(query)

    query.grouping_sets
    |> Enum.map(fn grouping_set ->
      grouping_set
      |> Enum.map(&(query.group_by |> Enum.at(&1) |> column_sql(dialect)))
      |> Enum.intersperse(", ")
    end)
    |> case do
      [[]] -> if dialect in [ClouderaImpala], do: [" GROUP BY NULL"], else: [" GROUP BY ()"]
      [grouping_set] -> [" GROUP BY ", grouping_set]
      grouping_sets -> [" GROUP BY GROUPING SETS ((", Enum.intersperse(grouping_sets, "), ("), "))"]
    end
  end

  defp group_by_fragments(_query), do: []

  defp having_fragments(%Query{subquery?: false}), do: []
  defp having_fragments(%Query{having: nil}), do: []
  defp having_fragments(query), do: [" HAVING ", column_sql(query.having, sql_dialect_module(query))]

  defp order_by_fragments(%Query{subquery?: true, order_by: [_ | _] = order_by} = query) do
    dialect = sql_dialect_module(query)

    order_by =
      for {expression, dir, nulls} <- order_by do
        column = expression |> Expression.unalias() |> column_sql(dialect)
        dialect.order_by(column, dir, nulls)
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

  defp cast(%Expression{kind: :function, name: {:cast, to}} = expression, to), do: expression
  defp cast({:distinct, expression}, to), do: {:distinct, cast(expression, to)}
  defp cast(expression, to), do: Expression.function({:cast, to}, [expression], to)

  # -------------------------------------------------------------------
  # Build subquery with join-timing protection
  # -------------------------------------------------------------------

  defp build_subquery(%{ast: query, join_timing_protection: :invalid_row}) do
    dialect = sql_dialect_module(query)

    from =
      case dialect do
        Cloak.DataSource.SqlBuilder.Oracle -> " FROM dual"
        _ -> ""
      end

    [
      "(",
      build_fragments(query),
      ") UNION ALL (SELECT ",
      query.db_columns
      |> Enum.map(&Table.invalid_value(&1.type))
      |> Enum.map(&constant_to_fragment(&1, dialect))
      |> Enum.join(", "),
      from,
      ")"
    ]
  end

  defp build_subquery(%{ast: query, join_timing_protection: :not_exists}) do
    [
      "(",
      build_fragments(query),
      ") UNION ALL (SELECT * FROM (",
      build_fragments(%Query{strip_filters(query) | limit: 1, offset: 0, order_by: []}),
      ") t WHERE NOT EXISTS(",
      build_fragments(%Query{query | order_by: []}),
      "))"
    ]
  end

  defp build_subquery(subquery), do: build_fragments(subquery.ast)

  defp strip_filters(query) do
    Query.Lenses.all_queries()
    |> Lens.filter(&(&1.type == :restricted))
    |> Lens.map(query, &%Query{&1 | where: nil, having: nil})
  end

  # -------------------------------------------------------------------
  # Mark boolean expressions
  # -------------------------------------------------------------------

  # Some backends (like Oracle) have limited support for boolean expressions.
  # A boolean expression is an expression consisting of conditions and boolean operators (`and`, `or`, `not`).
  # We wrap boolean expressions in non-filtering clauses inside a dummy `boolean_expression` function call.
  # This will result in no-op on backends where boolean expressions are supported and
  # in a `case` statement where they aren't.

  defp mark_boolean_expressions(query) do
    query
    |> update_in([non_filter_clauses()], &mark_boolean_expression(&1, :mark))
    |> update_in([Query.Lenses.filter_clauses()], &mark_boolean_expression(&1, :ignore))
  end

  defp non_filter_clauses() do
    Lens.multiple([
      Lens.keys?([:columns, :group_by, :db_columns]) |> Lens.all(),
      Lens.key?(:order_by) |> Lens.all() |> Lens.at(0)
    ])
  end

  defp mark_boolean_expression({:distinct, expression}, state),
    do: {:distinct, mark_boolean_expression(expression, state)}

  defp mark_boolean_expression(%Expression{kind: :function} = function, state) do
    cond do
      state == :mark and boolean_expression?(function) ->
        alias = function.alias
        function = function |> Expression.unalias() |> mark_boolean_expression(:ignore)
        %Expression{function | name: "boolean_expression", alias: alias, args: [function]}

      state == :ignore and function.name not in ~w(and not or) ->
        %Expression{function | args: Enum.map(function.args, &mark_boolean_expression(&1, :mark))}

      function.name == "case" ->
        args =
          function.args
          |> Enum.with_index()
          |> Enum.map(fn {arg, index} ->
            state = if rem(index, 2) == 0, do: :ignore, else: :mark
            mark_boolean_expression(arg, state)
          end)

        %Expression{function | args: args}

      true ->
        %Expression{function | args: Enum.map(function.args, &mark_boolean_expression(&1, state))}
    end
  end

  defp mark_boolean_expression(expression, _state), do: expression

  defp boolean_expression?(%Expression{kind: :function} = function),
    do: function.name in ~w(and or) or Function.condition?(function)
end
