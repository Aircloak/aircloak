defmodule Cloak.SqlQuery.Builder do
  @moduledoc """
  Provides functionality for constructing an SQL query from a
  `Cloak.SqlQuery.t' AST
  """

  alias Cloak.SqlQuery.Parsers.Token

  @typep query_spec :: {statement, [constant]}
  @typep constant :: String.t | number | boolean
  @typep statement :: iodata
  @typep fragment :: String.t | {:param, constant} | [fragment]


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(Cloak.SqlQuery.t) :: query_spec
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(%{from: table} = query) do
    fragments_to_query_spec([
      "SELECT ", Enum.map_join(ordered_selected_columns(query), ",", &select_column_to_string/1), " ",
      "FROM ", table, " ",
      where_fragments(query[:where])
    ])
  end

  @spec select_column_to_string(Cloak.SqlQuery.Parser.column) :: String.t
  @doc "Creates a string representation of a potentially complex column selection"
  def select_column_to_string({:count, :star}), do: "'*' as \"count(*)\""
  def select_column_to_string(column), do: column


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

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

  defp ordered_selected_columns(%{columns: columns} = query) do
    unselected_group_by_columns = Map.get(query, :group_by, []) -- columns
    columns ++ unselected_group_by_columns
  end

  defp where_fragments(nil), do: []
  defp where_fragments(where_clause) do
    ["WHERE ", where_clause_to_fragments(where_clause)]
  end

  defp where_clause_to_fragments([_|_] = and_clauses) do
    ["(", and_clauses |> Enum.map(&where_clause_to_fragments/1) |> join(" AND "), ")"]
  end
  defp where_clause_to_fragments({:comparison, what, comparator, value}) do
    [to_fragment(what), to_fragment(comparator), to_fragment(value)]
  end
  defp where_clause_to_fragments({:in, what, values}) do
    [to_fragment(what), " IN (", values |> Enum.map(&to_fragment/1) |> join(","), ")"]
  end
  Enum.each([
    {:like, " LIKE ", " NOT LIKE "},
    {:ilike, " ILIKE ", " NOT ILIKE "},
  ], fn({keyword, fragment, negative_fragment}) ->
    defp where_clause_to_fragments({unquote(keyword), what, match}) do
      [to_fragment(what), unquote(fragment), to_fragment(match)]
    end
    defp where_clause_to_fragments({:not, {unquote(keyword), what, match}}) do
      [to_fragment(what), unquote(negative_fragment), to_fragment(match)]
    end
  end)

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom)
  defp to_fragment(%Token{category: :constant, value: value}), do: {:param, value.value}

  defp join([first | [_|_] = rest], joiner), do: [first, joiner, join(rest, joiner)]
  defp join([el], _joiner), do: [el]
  defp join([], _joiner), do: []

end
