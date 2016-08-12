defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Aql.Query
  alias Cloak.Aql.Column
  alias Cloak.Aql.Parsers.Token

  @typep query_spec :: {statement, [constant]}
  @typep constant :: String.t | number | boolean
  @typep statement :: iodata
  @typep fragment :: String.t | {:param, constant} | [fragment]


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(Query.t) :: query_spec
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(%Query{mode: :unparsed} = query) do
    {:subquery, %{unparsed_string: unsafe_subquery}} = query.from

    {
      ["SELECT ", columns_sql(query.db_columns), " FROM (", unsafe_subquery, ") AS unsafe_subquery"],
      []
    }
  end
  def build(query) do
    query
    |> build_fragments()
    |> fragments_to_query_spec()
  end

  @doc "Returns a name uniquely identifying a column in the generated query."
  @spec column_name(Column.t) :: String.t
  def column_name(%Column{table: :unknown, name: name}), do: name
  def column_name(column), do: "#{column.table.name}.#{column.name}"


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp build_fragments(query) do
    [
      "SELECT ", columns_sql(query.db_columns), " ",
      "FROM ", from_clause(query.from, query), " ",
      where_fragments(query.where)
    ]
  end

  defp columns_sql(columns) do
    columns
    |> Enum.map(&column_sql/1)
    |> Enum.join(",")
  end

  defp column_sql(%Column{db_function: :coalesce, db_function_args: args}),
    do: "COALESCE(#{columns_sql(args)})"
  defp column_sql(column), do: column_name(column)

  defp from_clause({:join, join}, query) do
    ["(", from_clause(join.lhs, query), " ", join_sql(join.type), " ", from_clause(join.rhs, query),
      on_clause(join.conditions), ")"]
  end
  defp from_clause({:subquery, subquery}, _query) do
    ["(", build_fragments(subquery.ast), ") AS ", subquery.alias]
  end
  defp from_clause(table_name, query) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from()
  end

  defp on_clause([]), do: []
  defp on_clause([_|_] = conditions), do: [" ON ", conditions_to_fragments(conditions)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:full_outer_join), do: "FULL OUTER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}), do: table_name
  defp table_to_from(table), do: "#{table.db_name} AS #{table.name}"

  @spec fragments_to_query_spec([fragment]) :: query_spec
  defp fragments_to_query_spec(fragments) do
    {query_string(fragments), params(fragments)}
  end

  defp query_string(fragments) do
    fragments
    |> List.flatten()
    |> Enum.reduce(%{query_string: [], param_index: 1}, &parse_fragment(&2, &1))
    |> Map.fetch!(:query_string)
  end

  defp parse_fragment(query_builder, string) when is_binary(string) do
    %{query_builder | query_string: [query_builder.query_string, string]}
  end
  defp parse_fragment(query_builder, {:param, _value}) do
    %{query_builder |
      query_string: [query_builder.query_string, "$#{query_builder.param_index}"],
      param_index: query_builder.param_index + 1
    }
  end

  defp params(fragments) do
    fragments
    |> List.flatten()
    |> Stream.filter(&match?({:param, _}, &1))
    |> Enum.map(fn({:param, value}) -> value end)
  end

  defp where_fragments([]), do: []
  defp where_fragments(where_clause) do
    ["WHERE ", conditions_to_fragments(where_clause)]
  end

  defp conditions_to_fragments([_|_] = and_clauses) do
    ["(", and_clauses |> Enum.map(&conditions_to_fragments/1) |> join(" AND "), ")"]
  end
  defp conditions_to_fragments({:comparison, what, comparator, value}) do
    [to_fragment(what), to_fragment(comparator), to_fragment(value)]
  end
  defp conditions_to_fragments({:in, what, values}) do
    [to_fragment(what), " IN (", values |> Enum.map(&to_fragment/1) |> join(","), ")"]
  end
  defp conditions_to_fragments({:not, {:is, what, match}}) do
    [to_fragment(what), " IS NOT ", to_fragment(match)]
  end
  Enum.each([
    {:like, " LIKE "},
    {:ilike, " ILIKE "},
    {:is, " IS "},
  ], fn({keyword, fragment}) ->
    defp conditions_to_fragments({unquote(keyword), what, match}) do
      [to_fragment(what), unquote(fragment), to_fragment(match)]
    end
  end)

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%Token{category: :constant, value: value}), do: {:param, value.value}
  defp to_fragment(%Timex.DateTime{} = time), do: {:param, time}
  defp to_fragment(%{} = column), do: "#{column.table.name}.#{column.name}"

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]
end
