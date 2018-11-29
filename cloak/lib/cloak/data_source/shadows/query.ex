defmodule Cloak.DataSource.Shadows.Query do
  @moduledoc "Module for querying a datasource to build a shadow table."

  alias Cloak.Sql.{DataSource, Parser, Compiler}
  alias Cloak.Query.DbEmulator

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a shadow table for the given column. A shadow table is just a list of values from that column that appear for
  many users.
  """
  @spec build_shadow(DataSource.t(), String.t(), String.t()) :: [any]
  def build_shadow(data_source, table, column) do
    case user_id(data_source, table) do
      nil -> []
      ^column -> []
      user_id -> do_build_shadow(data_source, table, column, user_id)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_build_shadow(data_source, table, column, user_id) do
    """
      SELECT "#{column}"
      FROM "#{table}"
      GROUP BY 1
      HAVING COUNT(DISTINCT "#{user_id}") > #{min_users()}
      ORDER BY COUNT(*) DESC
      LIMIT #{size()}
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(data_source)
    |> DbEmulator.select()
    |> Enum.map(&hd/1)
  end

  defp user_id(data_source, table) do
    data_source.tables[String.to_existing_atom(table)].user_id
  end

  defp min_users(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:min_users)

  defp size(), do: Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:size)
end
