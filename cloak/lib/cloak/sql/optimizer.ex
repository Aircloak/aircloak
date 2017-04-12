defmodule Cloak.Sql.Optimizer do
  @moduledoc """
  Will rewrite queries into forms that make use of subqueries
  that can be offloaded to the database, in the cases where it
  is known to be safe.

  The optimizer is conservative, and will only apply optimisations
  known to be safe.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.{Parser, Optimizer.Helper}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Rewrites a query into an alternate form yielding better performance"
  @spec rewrite(Parser.parsed_query, DataSource.t) :: Parser.parsed_query
  def rewrite(query, data_source) do
    with true <- Helper.eligible(query),
        {:ok, uid_column} <- Helper.user_id_column(query[:from], data_source),
        {outer_clauses, inner_clauses} <- selectable_clauses(query) do

      optimized_query = query
      |> Map.put(:columns, outer_clauses)
      |> Map.put(:from, construct_inner_select(query, uid_column, inner_clauses))

      optimized_query
    else
      _ -> query
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selectable_clauses(query), do:
    List.foldr(query[:columns], {[], []}, fn(column, {outer_acc, inner_acc}) ->
      {outer, inner} = Helper.column_replacement(column)
      {[outer | outer_acc], [inner | inner_acc]}
    end)

  defp construct_inner_select(query, uid_column, columns) do
    {:unquoted, table_name} = query[:from]

    {:subquery,
      %{
        alias: table_name,
        ast: %{
          columns: [uid_column | columns],
          command: :select, distinct?: false, from: query[:from],
          group_by: [uid_column | query[:group_by] || []]
        },
        type: :parsed
      }
    }
  end
end
