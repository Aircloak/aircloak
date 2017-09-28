defmodule Cloak.Query.DataEngine do
  @moduledoc """
  Retrieval of data from the data source according to the query specification.

  Note: This module is currently a work in progress, but the description reflects the desired state
  """

  alias Cloak.Sql.{Condition, Function, Query}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Determines whether the query needs to be emulated or not."
  @spec needs_emulation?(Query.t) :: boolean
  def needs_emulation?(%Query{subquery?: false, from: table}) when is_binary(table), do: false
  def needs_emulation?(%Query{subquery?: true, from: table} = query) when is_binary(table), do:
    not query.data_source.driver.supports_query?(query) or has_emulated_expressions?(query)
  def needs_emulation?(query), do:
    not query.data_source.driver.supports_query?(query) or
    query |> get_in([Query.Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated?)) or
    (query.subquery? and has_emulated_expressions?(query)) or
    has_emulated_join_conditions?(query)

  @doc "Partitions where clauses to the ones which need to be emulated and the ones which don't."
  @spec partitioned_where_clauses(Query.t) :: {Query.where_clause, Query.where_clause}
  def partitioned_where_clauses(query) do
    Condition.partition(query.where,
      fn(condition) ->
        emulated_expression_condition?(condition) or
        (
          query.emulated? and
          (
            multiple_tables_condition?(condition) or
            not is_binary(query.from)
          )
        )
      end
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp emulated_expression?(expression), do:
    DataDecoder.needs_decoding?(expression) or Function.has_attribute?(expression, :emulated)

  defp emulated_expression_condition?(condition) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.any?(&emulated_expression?/1)
  end

  defp has_emulated_expressions?(query), do:
    Query.Lenses.all_expressions()
    |> Lens.to_list([query.columns, query.group_by, query.having, query.where])
    |> Enum.any?(&emulated_expression?/1)

  defp has_emulated_join_conditions?(query), do:
    query
    |> Helpers.all_join_conditions()
    |> get_in([Query.Lenses.all_expressions()])
    |> Enum.any?(&emulated_expression?/1)

  defp multiple_tables_condition?(condition) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> Enum.count() > 1
  end
end
