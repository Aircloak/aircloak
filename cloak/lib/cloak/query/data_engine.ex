defmodule Cloak.Query.DataEngine do
  @moduledoc "Retrieval of data from the data source according to the query specification."

  require Logger

  alias Cloak.Sql.{Compiler.Helpers, Condition, Query}
  alias Cloak.Query.{DataDecoder, DbEmulator}
  alias Cloak.DataSource


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
    Retrieves rows from the data source, emulating sub-queries if necessary, and applies the rows processor over them.
  """
  @spec select(Query.t, ((Enumerable.t) -> result)) :: result when result: var
  def select(%Query{emulated?: true} = query, rows_processor) do
    Logger.debug("Emulating query ...")
    query |> DbEmulator.select() |> rows_processor.()
  end
  def select(%Query{emulated?: false} = query, rows_processor) do
    DataSource.select!(%Query{query | where: offloaded_where(query)}, rows_processor)
  end

  @doc "Determines whether the query needs to be emulated or not."
  @spec needs_emulation?(Query.t) :: boolean
  def needs_emulation?(query), do:
    not query.data_source.driver.supports_query?(query) or
    query |> get_in([Query.Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated?)) or
    (query.subquery? and has_emulated_expressions?(query)) or
    has_emulated_join_conditions?(query)

  @doc "Returns the where clauses that can be applied by the data source."
  @spec offloaded_where(Query.t) :: Query.where_clause
  def offloaded_where(query), do:
    Condition.reject(query.where, &emulated_condition?(&1, query))

  @doc "Returns the where clauses that must be applied by inside the cloak."
  @spec emulated_where(Query.t) :: Query.where_clause
  def emulated_where(query), do:
    Condition.reject(query.where, &not emulated_condition?(&1, query))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp emulated_condition?(condition, query) do
    emulated_expression_condition?(condition, query.data_source) or
    (
      query.emulated? and
      (
        multiple_tables_condition?(condition) or
        not is_binary(query.from)
      )
    )
  end

  defp emulated_expression?(expression, data_source), do:
    DataDecoder.needs_decoding?(expression) or
    (expression.function? and not data_source.driver.supports_function?(expression, data_source))

  defp emulated_expression_condition?(condition, data_source) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.any?(&emulated_expression?(&1, data_source))
  end

  defp has_emulated_expressions?(query), do:
    Query.Lenses.all_expressions()
    |> Lens.to_list([query.columns, query.group_by, query.having, query.where])
    |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp has_emulated_join_conditions?(query), do:
    query
    |> Helpers.all_join_conditions()
    |> get_in([Query.Lenses.all_expressions()])
    |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp multiple_tables_condition?(condition) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> Enum.count() > 1
  end
end
