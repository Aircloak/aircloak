defmodule Cloak.DataSource.Shadows.Query do
  alias Cloak.Sql.{Parser, Compiler}
  alias Cloak.Query.DbEmulator

  @min_distinct_users 10
  @max_values 100

  def build_shadow(data_source, table, column) do
    """
      SELECT "#{column}"
      FROM "#{table}"
      GROUP BY 1
      HAVING COUNT(DISTINCT "#{user_id(data_source, table)}") > #{@min_distinct_users}
      ORDER BY COUNT(*)
      LIMIT #{@max_values}
    """
    |> Parser.parse!()
    |> Compiler.compile_direct!(data_source)
    |> DbEmulator.select()
    |> List.flatten()
  end

  defp user_id(data_source, table), do: data_source.tables[String.to_existing_atom(table)].user_id
end
