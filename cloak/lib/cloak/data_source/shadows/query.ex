defmodule Cloak.DataSource.Shadows.Query do
  @moduledoc "Module for querying a datasource to build a shadow table."

  alias Cloak.Sql.{DataSource, Parser, Compiler}
  alias Cloak.Query.DbEmulator

  @min_distinct_users 10
  @max_values 100

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a shadow table for the given column. A shadow table is just a list of values from that column that appear for
  many users.
  """
  @spec build_shadow(DataSource.t(), String.t(), String.t()) :: [any]
  def build_shadow(data_source, table, column) do
    """
      SELECT "#{column}"
      FROM "#{table}"
      GROUP BY 1
      HAVING COUNT(DISTINCT "#{user_id(data_source, table)}") > #{@min_distinct_users}
      ORDER BY COUNT(*) DESC
      LIMIT #{@max_values}
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(data_source)
    |> DbEmulator.select()
    |> Enum.map(&hd/1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
