defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Aql.Query
  alias Cloak.Aql.Column
  alias Cloak.DataSource.SqlBuilder.DbFunction


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(atom, Query.t) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(_sql_dialect, %Query{mode: :unparsed} = query) do
    {:subquery, %{unparsed_string: unsafe_subquery}} = query.from
    to_string(["SELECT ", columns_sql(query.db_columns), " FROM (", unsafe_subquery, ") AS unsafe_subquery"])
  end
  def build(sql_dialect, query) do
    build_fragments(sql_dialect, query) |> to_string()
  end

  @doc "Returns a name uniquely identifying a column in the generated query."
  @spec column_name(Column.t) :: String.t
  def column_name(%Column{table: :unknown, name: name}), do: "\"#{name}\""
  def column_name(column), do: "\"#{column.table.name}\".\"#{column.name}\""


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp build_fragments(sql_dialect, query) do
    [
      "SELECT ", columns_sql(query.db_columns), " ",
      "FROM ", from_clause(sql_dialect, query.from, query), " ",
      where_fragments(sql_dialect, query.where),
      group_by_fragments(query)
    ]
  end

  defp columns_sql(columns) do
    columns
    |> Enum.map(&column_sql/1)
    |> Enum.intersperse(?,)
  end

  defp column_sql(:*), do: "*"
  defp column_sql({:distinct, column}),
    do: ["DISTINCT ", column_sql(column)]
  defp column_sql(%Column{alias: alias} = column) when alias != nil do
    [column_sql(%Column{column | alias: nil}), "AS ", alias]
  end
  defp column_sql(%Column{db_function: fun_name, db_function_args: args, type: type}) when fun_name != nil,
    do: DbFunction.sql(fun_name, Enum.map(args, &column_sql/1), type)
  defp column_sql(%Column{constant?: true, value: value, type: type}),
    do: DbFunction.sql({:cast, type}, [constant_to_fragment(value)], type)
  defp column_sql(column), do: column_name(column)

  defp from_clause(sql_dialect, {:join, join}, query) do
    ["(", from_clause(sql_dialect, join.lhs, query), " ", join_sql(join.type), " ",
      from_clause(sql_dialect, join.rhs, query), on_clause(sql_dialect, join.conditions), ")"]
  end
  defp from_clause(sql_dialect, {:subquery, subquery}, _query) do
    ["(", build_fragments(sql_dialect, subquery.ast), ") AS ", subquery.alias]
  end
  defp from_clause(_sql_dialect, table_name, query) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from()
  end

  defp on_clause(_sql_dialect, []), do: []
  defp on_clause(sql_dialect, conditions) when is_list(conditions),
    do: [" ON ", conditions_to_fragments(sql_dialect, conditions)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:full_outer_join), do: "FULL OUTER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}), do: table_name
  defp table_to_from(table), do: "#{table.db_name} AS \"#{table.name}\""

  defp where_fragments(_sql_dialect, []), do: []
  defp where_fragments(sql_dialect, where_clause) do
    ["WHERE ", conditions_to_fragments(sql_dialect, where_clause)]
  end

  defp conditions_to_fragments(sql_dialect, and_clauses) when is_list(and_clauses) do
    ["(", and_clauses |> Enum.map(&conditions_to_fragments(sql_dialect, &1)) |> join(" AND "), ")"]
  end
  defp conditions_to_fragments(_sql_dialect, {:comparison, what, comparator, value}) do
    [to_fragment(what), to_fragment(comparator), to_fragment(value)]
  end
  defp conditions_to_fragments(_sql_dialect, {:in, what, values}) do
    [to_fragment(what), " IN (", values |> Enum.map(&to_fragment/1) |> join(", "), ")"]
  end
  defp conditions_to_fragments(_sql_dialect, {:not, {:is, what, match}}) do
    [to_fragment(what), " IS NOT ", to_fragment(match)]
  end
  defp conditions_to_fragments(_sql_dialect, {:like, what, match}) do
    [to_fragment(what), " LIKE ", to_fragment(match)]
  end
  defp conditions_to_fragments(_sql_dialect, {:ilike, what, match}) do
    [to_fragment(what), " ILIKE ", to_fragment(match)]
  end
  defp conditions_to_fragments(_sql_dialect, {:is, what, match}) do
    [to_fragment(what), " IS ", to_fragment(match)]
  end

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%NaiveDateTime{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Time{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Date{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Column{constant?: true, value: value}), do: constant_to_fragment(value)
  defp to_fragment(%Column{} = column), do: "\"#{column.table.name}\".\"#{column.name}\""

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp constant_to_fragment(value) when is_binary(value), do: [?', escape_string(value), ?']
  defp constant_to_fragment(value) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value) when is_boolean(value), do: to_string(value)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}) do
    [
      "GROUP BY ",
      group_by |> Enum.map(&column_sql/1) |> Enum.intersperse(",")
    ]
  end
  defp group_by_fragments(_query), do: []
end
