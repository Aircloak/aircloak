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
  def build(%{mode: :unparsed, from: {:subquery, unsafe_select}} = query) do
    {
      ["SELECT ", columns_string(query), " FROM (", unsafe_select, ") AS unsafe_subquery"],
      []
    }
  end
  def build(query) do
    fragments_to_query_spec([
      "SELECT ", columns_string(query), " ",
      "FROM ", from_clause(query), " ",
      where_fragments(query[:where])
    ])
  end


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp columns_string(query) do
    query.db_columns
    |> Enum.map(&Cloak.SqlQuery.Column.alias/1)
    |> Enum.join(",")
  end

  defp from_clause(query) do
    query.selected_tables
    |> Enum.map(&(&1.db_name))
    |> Enum.join(" CROSS JOIN ")
  end

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
  defp where_clause_to_fragments({:not, {:is, what, match}}) do
    [to_fragment(what), " IS NOT ", to_fragment(match)]
  end
  Enum.each([
    {:like, " LIKE "},
    {:ilike, " ILIKE "},
    {:is, " IS "},
  ], fn({keyword, fragment}) ->
    defp where_clause_to_fragments({unquote(keyword), what, match}) do
      [to_fragment(what), unquote(fragment), to_fragment(match)]
    end
  end)

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%Token{category: :constant, value: value}), do: {:param, value.value}
  defp to_fragment(%Timex.DateTime{} = time), do: {:param, time}
  defp to_fragment(%{} = column), do: "#{column.table.db_name}.#{column.name}"

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]
end
