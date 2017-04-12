defmodule Cloak.Sql.Optimizer.Helper do
  @moduledoc """
  Utility methods split out into a standalone module
  for easier testing.
  """

  alias Cloak.Sql.{Parser, Function}

  @supported_aggregates ["count", "sum", "min", "max"]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Determines whether or not a query is eligible for optimization"
  @spec eligible(Parser.parsed_query) :: boolean
  def eligible(query), do:
    from_single_table(query) and
    has_aggregate_function(query) and
    supported_aggregates(query) and
    no_nonaggregate_functions(query) and
    no_where_clauses(query)

  @doc """
  Returns the user id column for a table in the form it would
  have been returned by the parser, had it been typed out by
  the analyst.
  """
  @spec user_id_column(Parser.table, DataSource.t) :: {:ok, Parser.unqualified_identifier} | :not_found
  def user_id_column({:unquoted, table_name}, data_source) do
    tables = data_source[:tables]
    case Map.get(tables, String.to_atom(table_name)) do
      nil -> :not_found
      table -> {:ok, {:identifier, :unknown, {:unquoted, table[:user_id]}}}
    end
  end


  # -------------------------------------------------------------------
  # Internal function
  # -------------------------------------------------------------------

  defp no_where_clauses(query), do: is_nil(query[:where]) or query[:where] === []

  defp from_single_table(query), do:
    match?({:unquoted, _}, query[:from])

  defp has_aggregate_function(query), do: aggregate_functions(query) !== []

  defp no_nonaggregate_functions(query), do: nonaggregate_functions(query) === []

  defp supported_aggregates(query), do:
    aggregate_functions(query)
    |> Enum.map(fn({:function, name, _}) -> name end)
    |> Enum.all?(& Enum.member?(@supported_aggregates, &1))

  defp functions(query), do:
    query[:columns]
    |> Enum.map(fn
      ({something, :as, _other}) -> something
      (other) -> other
    end)
    |> Enum.filter(& Function.function?(&1))
    |> Enum.filter(& Function.exists?(&1))

  defp aggregate_functions(query), do:
    functions(query)
    |> Enum.filter(& Function.has_attribute?(&1, :aggregator))

  defp nonaggregate_functions(query), do:
    functions(query)
    |> Enum.reject(& Function.has_attribute?(&1, :aggregator))

  defp generate_alias(name), do: "#{name}_alias_#{System.unique_integer([:positive])}"
end
