defmodule Cloak.DataSource.Shadows.Query do
  @moduledoc "Module for querying a datasource to build a shadow table."

  alias Cloak.Sql.{DataSource, Parser, Compiler}
  alias Cloak.Query.DbEmulator
  alias Cloak.DataSource.SqlBuilder

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a shadow table for the given column. A shadow table is just a list of values from that column that appear for
  many users.
  """
  @spec build_shadow(DataSource.t(), String.t(), String.t()) :: [any]
  def build_shadow(data_source, table_name, column) do
    table_name = String.to_existing_atom(table_name)
    table = data_source.tables[table_name]

    cond do
      table.user_id_join_chain == nil -> []
      table.keys[column] == :user_id -> []
      true -> maybe_build_shadow(data_source, table_name, column)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_build_shadow(data_source, table_name, column) do
    if should_maintain_shadow?(data_source, table_name) do
      {user_id, table_chain} = SqlBuilder.build_table_chain_with_user_id(data_source.tables, table_name)

      """
        SELECT "#{table_name}"."#{column}"
        FROM #{table_chain}
        GROUP BY 1
        HAVING COUNT(DISTINCT #{user_id}) > #{min_users()}
        ORDER BY COUNT(*) DESC
        LIMIT #{size()}
      """
      |> Parser.parse!()
      |> Compiler.compile_direct!(nil, data_source)
      |> DbEmulator.compile()
      |> DbEmulator.select()
      |> Enum.map(&hd/1)
    else
      []
    end
  end

  defp should_maintain_shadow?(data_source, table_name),
    do: Map.get(data_source.tables[table_name], :maintain_shadow_db, true)

  defp min_users(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:min_users)

  defp size(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:size)
end
