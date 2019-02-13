defmodule Cloak.DataSource.Isolators.Query do
  @moduledoc "Utility for checking if a column is isolating. See `Isolating columns` in anonymization.md for details."

  alias Cloak.Sql.{Parser, Compiler}
  alias Cloak.DataSource
  alias Cloak.DataSource.SqlBuilder
  alias Cloak.Query.DbEmulator

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table_name, column) do
    table_name = String.to_existing_atom(table_name)
    table = data_source.tables[table_name]

    cond do
      table.user_id_join_chain == nil -> nil
      table.keys[column] == :user_id -> true
      true -> isolating_ratio(data_source, table_name, column) > threshold()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp isolating_ratio(data_source, table_name, column) do
    {user_id, table_chain} = SqlBuilder.build_table_chain_with_user_id(data_source.tables, table_name)

    """
      SELECT isolating, COUNT(*) FROM (
        SELECT BOOL_OP('=', MAX(#{user_id}), MIN(#{user_id})) AS isolating
        FROM #{table_chain}
        GROUP BY "#{table_name}"."#{column}"
      ) x
      WHERE isolating IS NOT NULL
      GROUP BY 1
      ORDER BY 1
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(nil, data_source)
    |> DbEmulator.compile()
    |> DbEmulator.select()
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
end
