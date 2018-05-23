defmodule Cloak.DataSource.Isolators.Query do
  @moduledoc "Utility for checking if a column is isolating."

  alias Cloak.Sql.{Parser, Compiler}
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), atom, String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    isolating_values(data_source, table, column) / (unique_values(data_source, table, column) + 1) > threshold()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp isolating_values(data_source, table, column) do
    %{user_id: user_id} = data_source.tables[table]

    """
      SELECT COUNT(*) FROM (
        SELECT #{column}
        FROM #{table}
        GROUP BY #{column}
        HAVING COUNT(#{user_id}) = 1
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
end
