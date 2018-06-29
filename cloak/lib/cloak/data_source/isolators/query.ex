defmodule Cloak.DataSource.Isolators.Query do
  @moduledoc "Utility for checking if a column is isolating. See `Isolating columns` in anonymization.md for details."

  alias Cloak.Sql.{Parser, Compiler}
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    if column == user_id(data_source, table) do
      true
    else
      isolating_values(data_source, table, column) / (unique_values(data_source, table, column) + 1) > threshold()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp isolating_values(data_source, table, column) do
    # The keep field is needed for backends that don't allow an empty SELECT list, like SQLServer.
    # It needs to be used in the outer query so that it's not removed by the Optimizer.
    """
      SELECT COUNT(keep) FROM (
        SELECT 1 AS keep
        FROM #{table}
        GROUP BY "#{column}"
        HAVING COUNT(DISTINCT "#{user_id(data_source, table)}") = 1
      ) x
    """
    |> select_one!(data_source)
  end

  defp unique_values(data_source, table, column) do
    """
      SELECT COUNT(keep) FROM (
        SELECT 1 AS keep
        FROM #{table}
        GROUP BY "#{column}"
      ) x
    """
    |> select_one!(data_source)
  end

  defp select_one!(query, data_source) do
    [[result]] =
      query
      |> Parser.parse!()
      |> Compiler.compile_direct!(data_source)
      |> Cloak.Query.DbEmulator.select()

    result
  end

  defp threshold(), do: Application.fetch_env!(:cloak, :anonymizer) |> Keyword.fetch!(:isolating_column_threshold)

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
