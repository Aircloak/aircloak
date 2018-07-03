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
    case user_id(data_source, table) do
      nil -> false
      ^column -> true
      _ -> isolating_ratio(data_source, table, column) > threshold()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp isolating_ratio(data_source, table, column) do
    [non_isolating_values, unique_values] =
      """
        SELECT SUM(CAST(non_isolating AS integer)), COUNT(*) FROM (
          SELECT CAST(COUNT(DISTINCT "#{user_id(data_source, table)}") - 1 AS boolean) AS non_isolating
          FROM #{table}
          GROUP BY "#{column}"
        ) x
      """
      |> select_one!(data_source)

    (unique_values - (non_isolating_values || 0)) / (unique_values + 1)
  end

  defp select_one!(query, data_source) do
    [result] =
      query
      |> Parser.parse!()
      |> Compiler.compile_direct!(data_source)
      |> Cloak.Query.DbEmulator.select()

    result
  end

  defp threshold(), do: Application.fetch_env!(:cloak, :anonymizer) |> Keyword.fetch!(:isolating_column_threshold)

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
