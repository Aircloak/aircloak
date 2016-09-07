defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Aql.Query
  alias Cloak.Aql.Column
  alias Cloak.DataSource.SqlBuilder.DbFunction

  defmodule SqlBuildError do
    @moduledoc false
    defexception message: "Error during sql query build."
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(Query.t, atom) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(%Query{mode: :unparsed} = query, sql_dialect) do
    {:subquery, %{unparsed_string: unsafe_subquery}} = query.from
    [
      "SELECT ", columns_sql(query.db_columns, sql_dialect),
      " FROM (", unsafe_subquery, ") AS unsafe_subquery"
    ] |> to_string()
  end
  def build(query, sql_dialect) do
    query |> build_fragments(sql_dialect) |> to_string()
  end

  @doc "Returns a name uniquely identifying a column in the generated query."
  @spec column_name(Column.t, atom) :: String.t
  def column_name(%Column{table: :unknown, name: name}, :drill), do: "`#{name}`"
  def column_name(column,  :drill), do: "`#{column.table.name}`.`#{column.name}`"
  def column_name(%Column{table: :unknown, name: name}, _sql_dialect), do: "\"#{name}\""
  def column_name(column, _sql_dialect), do: "\"#{column.table.name}\".\"#{column.name}\""


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp build_fragments(query, sql_dialect) do
    [
      "SELECT ", columns_sql(query.db_columns, sql_dialect), " ",
      "FROM ", from_clause(query.from, query, sql_dialect), " ",
      where_fragments(query.where, sql_dialect),
      group_by_fragments(query, sql_dialect)
    ]
  end

  defp columns_sql(columns, sql_dialect) do
    columns
    |> Enum.map(&column_sql(&1, sql_dialect))
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*, _sql_dialect), do: "*"
  defp column_sql({:distinct, column}, sql_dialect), do: ["DISTINCT ", column_sql(column, sql_dialect)]
  defp column_sql(%Column{alias: alias} = column, sql_dialect) when alias != nil,
    do: [column_sql(%Column{column | alias: nil}, sql_dialect), "AS ", alias]
  defp column_sql(%Column{db_function: fun_name, db_function_args: args, type: type}, sql_dialect)
    when fun_name != nil, do: DbFunction.sql(fun_name, Enum.map(args, &column_sql(&1, sql_dialect)), type)
  defp column_sql(%Column{constant?: true, value: value, type: type}, _sql_dialect),
    do: DbFunction.sql({:cast, type}, [constant_to_fragment(value)], type)
  defp column_sql(column, sql_dialect), do: column_name(column, sql_dialect)

  defp from_clause({:join, join}, query, sql_dialect) do
    ["(", from_clause(join.lhs, query, sql_dialect), " ", join_sql(join.type), " ",
      from_clause(join.rhs, query, sql_dialect), on_clause(join.conditions, sql_dialect), ")"]
  end
  defp from_clause({:subquery, subquery}, _query, sql_dialect) do
    ["(", build_fragments(subquery.ast, sql_dialect), ") AS ", subquery.alias]
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

  defp table_to_from(%{name: table_name, db_name: table_name}, _sql_dialect), do: table_name
  defp table_to_from(table, :drill), do: "#{table.db_name} AS `#{table.name}`"
  defp table_to_from(table, _sql_dialect), do: "#{table.db_name} AS \"#{table.name}\""

  defp where_fragments([], _sql_dialect), do: []
  defp where_fragments(where_clause, sql_dialect),
    do: ["WHERE ", conditions_to_fragments(where_clause, sql_dialect)]

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
    raise SqlBuildError, message:
      "'#{condition}' conditions are not implemented for '#{sql_dialect}' data sources."
  end

  defp to_fragment(string, _sql_dialect) when is_binary(string), do: string
  defp to_fragment(atom, _sql_dialect) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%NaiveDateTime{} = value, _sql_dialect), do: [?', to_string(value), ?']
  defp to_fragment(%Time{} = value, _sql_dialect), do: [?', to_string(value), ?']
  defp to_fragment(%Date{} = value, _sql_dialect), do: [?', to_string(value), ?']
  defp to_fragment(%Column{constant?: true, value: value}, _sql_dialect), do: constant_to_fragment(value)
  defp to_fragment(%Column{} = column, sql_dialect), do: column_name(column, sql_dialect)

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp constant_to_fragment(value) when is_binary(value), do: [?', escape_string(value), ?']
  defp constant_to_fragment(value) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value) when is_boolean(value), do: to_string(value)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}, sql_dialect),
    do: ["GROUP BY ", group_by |> Enum.map(&column_sql(&1, sql_dialect)) |> Enum.intersperse(",")]
  defp group_by_fragments(_query, _sql_dialect), do: []
end
