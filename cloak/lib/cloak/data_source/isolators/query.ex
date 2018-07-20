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
    uid_column = user_id(data_source, table)

    """
      SELECT isolating, COUNT(*) FROM (
        SELECT BOOL_OP('=', MAX(#{uid_column}), MIN(#{uid_column})) AS isolating
        FROM #{table}
        GROUP BY "#{column}"
      ) x
      GROUP BY 1
      ORDER BY 1
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(data_source)
    |> Cloak.Query.DbEmulator.select()
    |> case do
      [[false, non_isolating_values], [true, isolating_values]] ->
        isolating_values / (isolating_values + non_isolating_values + 1)

      [[true, _isolating_values]] ->
        1.0

      [[false, _non_isolating_values]] ->
        0.0

      [] ->
        0.0
    end
  end

  defp threshold(), do: Application.fetch_env!(:cloak, :anonymizer) |> Keyword.fetch!(:isolating_column_threshold)

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
