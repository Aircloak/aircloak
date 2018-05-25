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
    """
      SELECT COUNT(*) FROM (
        SELECT #{column}
        FROM #{table}
        GROUP BY #{column}
        HAVING COUNT(#{user_id(data_source, table)}) = 1
      ) x
    """
    |> select_count(data_source)
  end

  defp unique_values(data_source, table, column) do
    """
      SELECT COUNT(*) FROM (
        SELECT #{column}
        FROM #{table}
        GROUP BY #{column}
      ) x
    """
    |> select_count(data_source)
  end

  defp select_count(query, data_source) do
    query
    |> Parser.parse!()
    |> Compiler.compile_standard!(data_source)
    |> DataSource.select!(&Enum.concat/1)
    |> Enum.count()
  end

  defp threshold(), do: Application.fetch_env!(:cloak, :anonymizer) |> Keyword.fetch!(:isolating_column_threshold)

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
