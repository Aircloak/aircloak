defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Aql.Query
  alias Cloak.Aql.Column
  alias Cloak.DataSource.SqlBuilder.DbFunction
  alias Cloak.Query.Runner.RuntimeError


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(Query.t, atom) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(query, :mysql = sql_dialect) do
    # MySQL and MariaDB do not support FULL joins, so we have to split it into LEFT and RIGHT joins
    # see: http://www.xaprb.com/blog/2006/05/26/how-to-write-full-outer-join-in-mysql/
    case split_full_outer_join(query.from) do
      {:union, left_join, right_join} ->
        [%Column{db_function: "coalesce", function_args: [first_id | _]} | _] = query.db_columns
        query1 = %Query{query | from: left_join}
        query2 = %Query{query | from: right_join, where: query.where ++ [{:is, first_id, :null}]}
        build(query1, sql_dialect) <> " UNION ALL " <> build(query2, sql_dialect)
      _ -> query |> build_fragments(sql_dialect) |> to_string()
    end
  end
  def build(query, sql_dialect) do
    query |> build_fragments(sql_dialect) |> to_string()
  end

  @doc "Returns a name uniquely identifying a column in the generated query."
  @spec column_name(Column.t, atom) :: String.t
  def column_name(%Column{table: :unknown, name: name}, sql_dialect), do: quote_name(name, sql_dialect)
  def column_name(column, sql_dialect), do:
    "#{quote_name(column.table.name, sql_dialect)}.#{quote_name(column.name, sql_dialect)}"


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

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

  defp distinct(%Query{distinct?: true}), do: "DISTINCT "
  defp distinct(%Query{distinct?: false}), do: ""

  defp columns_sql(columns, sql_dialect) do
    columns
    |> Enum.map(&column_sql(&1, sql_dialect))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _sql_dialect), do: "*"
  defp column_sql({:distinct, column}, sql_dialect), do: ["DISTINCT ", column_sql(column, sql_dialect)]
  defp column_sql(%Column{alias: alias} = column, sql_dialect) when alias != nil and alias != "",
    do: [column_sql(%Column{column | alias: nil}, sql_dialect), " AS ", quote_name(alias, sql_dialect)]
  defp column_sql(%Column{db_function: fun_name, function_args: args, type: type}, sql_dialect)
    when fun_name != nil, do: DbFunction.sql(fun_name,
      Enum.map(args, &column_sql(&1, sql_dialect)), type, sql_dialect)
  defp column_sql(%Column{constant?: true, value: value}, _sql_dialect), do: constant_to_fragment(value)
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

  defp on_clause([], _sql_dialect), do: []
  defp on_clause(conditions, sql_dialect) when is_list(conditions),
    do: [" ON ", conditions_to_fragments(conditions, sql_dialect)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:full_outer_join), do: "FULL OUTER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}, sql_dialect), do:
    quote_name(table_name, sql_dialect)
  defp table_to_from(table, sql_dialect), do:
    "#{table.db_name} AS #{quote_name(table.name, sql_dialect)}"

  defp where_fragments([], _sql_dialect), do: []
  defp where_fragments(where_clause, sql_dialect),
    do: [" WHERE ", conditions_to_fragments(where_clause, sql_dialect)]

  defp conditions_to_fragments(and_clauses, sql_dialect) when is_list(and_clauses),
    do: ["(", and_clauses |> Enum.map(&conditions_to_fragments(&1, sql_dialect)) |> join(" AND "), ")"]
  defp conditions_to_fragments({:comparison, what, comparator, value}, sql_dialect),
    do: [to_fragment(what, sql_dialect), to_fragment(comparator, sql_dialect), to_fragment(value, sql_dialect)]
  defp conditions_to_fragments({:in, what, values}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " IN (",
      Enum.map(values, &to_fragment(&1, sql_dialect)) |> join(", "), ")"]
  defp conditions_to_fragments({:not, {:is, what, match}}, sql_dialect),
    do: [to_fragment(what, sql_dialect), " IS NOT ", to_fragment(match, sql_dialect)]
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
  defp conditions_to_fragments({condition, _what, _match}, sql_dialect) do
    condition = condition |> to_string() |> String.upcase()
    raise RuntimeError, message:
      "'#{condition}' conditions are not supported on '#{sql_dialect}' data sources."
  end

  defp to_fragment(string, _sql_dialect) when is_binary(string), do: string
  defp to_fragment(atom, _sql_dialect) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%Column{constant?: true, value: value}, _sql_dialect), do: constant_to_fragment(value)
  defp to_fragment(%Column{} = column, sql_dialect), do: column_sql(column, sql_dialect)

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp constant_to_fragment(%NaiveDateTime{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Time{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(%Date{} = value), do: [?', to_string(value), ?']
  defp constant_to_fragment(value) when is_binary(value), do: [?', escape_string(value), ?']
  defp constant_to_fragment(value) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value) when is_boolean(value), do: to_string(value)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}, sql_dialect),
    do: [" GROUP BY ", group_by |> Enum.map(&column_sql(&1, sql_dialect)) |> Enum.intersperse(", ")]
  defp group_by_fragments(_query, _sql_dialect), do: []

  defp having_fragments(%Query{subquery?: true, having: [_|_] = and_clauses}, sql_dialect),
    do: [" HAVING ", and_clauses |> Enum.map(&conditions_to_fragments(&1, sql_dialect)) |> join(" AND ")]
  defp having_fragments(_query, _sql_dialect), do: []

  defp order_by_fragments(%Query{subquery?: true, order_by: [_|_] = order_by} = query, sql_dialect) do
    order_by = for {index, dir} <- order_by do
      dir = if dir == :desc do " DESC" else " ASC" end
      name = query.db_columns |> Enum.at(index) |> Map.get(:alias) |> quote_name(sql_dialect)
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
    raise RuntimeError, message: "LIMIT clause is not supported on '#{sql_dialect}' data sources."
  defp range_fragments(%Query{subquery?: true, offset: offset}, sql_dialect) when offset > 0, do:
    raise RuntimeError, message: "OFFSET clause is not supported on '#{sql_dialect}' data sources."
  defp range_fragments(_query, _sql_dialect), do: []

  defp split_full_outer_join({:join, %{type: :full_outer_join} = join}) do
    left_join = {:join, %{join | type: :left_outer_join}}
    right_join = {:join, %{join | type: :right_outer_join}}
    {:union, left_join, right_join}
  end
  defp split_full_outer_join({:join, join}) do
    case split_full_outer_join(join.lhs) do
      {:union, left_join, right_join} ->
        left_join = {:join, %{join | lhs: left_join}}
        right_join = {:join, %{join | lhs: right_join}}
        {:union, left_join, right_join}
      _ ->
        case split_full_outer_join(join.rhs) do
          {:union, left_join, right_join} ->
            left_join = {:join, %{join | rhs: left_join}}
            right_join = {:join, %{join | rhs: right_join}}
            {:union, left_join, right_join}
          _ ->
            {:join, join}
        end
    end
  end
  defp split_full_outer_join(from), do: from
end
